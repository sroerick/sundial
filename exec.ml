(* The executor: fork/exec in a fresh process group, a deadline enforced on the
   whole group, and output to a spool file the child cannot fill.

   A pipe deadlocks a chatty job once 64 KiB is unread; a spool file costs one
   disk write. At reap only the tail (8 KiB) and the true byte count go into
   the record. *)

type result = {
  pid : int;
  started : float;
  finished : float;
  exit : int option;
  signal : int option;
  verdict : Spec.box_verdict;
  log_tail : string;
  log_bytes : int;
  timed_out : bool;
}

let counter = ref 0

let spool_path dir name =
  incr counter;
  Filename.concat dir
    (Printf.sprintf "%s.%d.%d.spool" name (Unix.getpid ()) !counter)

let kill_group pid signal =
  try Unix.kill (-pid) signal with Unix.Unix_error _ -> ()

(* Tail only: a 1 GiB job must not be read into memory to record 8 KiB. *)
let read_tail path cap =
  let st = Unix.stat path in
  let total = st.Unix.st_size in
  let len = Stdlib.min cap total in
  let buf = Bytes.create len in
  let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      ignore (Unix.lseek fd (total - len) Unix.SEEK_SET);
      let rec fill off =
        if off < len then
          let n = Unix.read fd buf off (len - off) in
          if n > 0 then fill (off + n)
      in
      fill 0);
  (Bytes.sub_string buf 0 len, total)

let run ?(grace = 10.) ?(spool_dir = "/var/db/pp-cron/spool")
    ?(ok_codes : int list = []) ?(env : (string * string) list option = None)
    ?(name = "run") ~(timeout : float) ~(cmd : string list) () : result =
  let prog =
    match cmd with
    | [] -> invalid_arg "Exec.run: empty cmd"
    | p :: _ -> p
  in
  let argv = Array.of_list cmd in
  let envp =
    match env with
    | None -> Unix.environment ()
    | Some bindings ->
      Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) bindings)
  in
  let spool = spool_path spool_dir name in
  let fd = Unix.openfile spool [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let started = Unix.gettimeofday () in
  let timed_out = ref false in
  let pid = Unix.fork () in
  if pid = 0 then begin
    (try ignore (Unix.setsid ()) with Unix.Unix_error _ -> ());
    Unix.dup2 fd Unix.stdout;
    Unix.dup2 fd Unix.stderr;
    Unix.close fd;
    try Unix.execvpe prog argv envp with _ -> Unix._exit 127
  end;
  Unix.close fd;
  let rec reap () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
      if Unix.gettimeofday () -. started >= timeout then terminate ()
      else begin
        Unix.sleepf 0.01;
        reap ()
      end
    | _, status -> status
  and terminate () =
    timed_out := true;
    kill_group pid Sys.sigterm;
    let deadline = Unix.gettimeofday () +. grace in
    let rec wait_kill () =
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | 0, _ ->
        if Unix.gettimeofday () >= deadline then begin
          kill_group pid Sys.sigkill;
          let _, status = Unix.waitpid [] pid in
          status
        end
        else begin
          Unix.sleepf 0.02;
          wait_kill ()
        end
      | _, status -> status
    in
    wait_kill ()
  in
  let status = reap () in
  let finished = Unix.gettimeofday () in
  let exit, signal =
    match status with
    | Unix.WEXITED n -> (Some n, None)
    | Unix.WSIGNALED s -> (None, Some s)
    | Unix.WSTOPPED s -> (None, Some s)
  in
  let verdict =
    if !timed_out then `Timeout
    else
      match exit with
      | Some n -> Policies.verdict_of_exit ~ok_codes n
      | None ->
        `Failed (Printf.sprintf "signal %d" (Option.value signal ~default:0))
  in
  let log_tail, log_bytes = read_tail spool Spec.log_tail_cap in
  (try Sys.remove spool with Sys_error _ -> ());
  { pid;
    started;
    finished;
    exit;
    signal;
    verdict;
    log_tail;
    log_bytes;
    timed_out = !timed_out }
