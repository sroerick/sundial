(* pp-cron scheduler and executor (card 9f2fd78a, S2).

   One tick fires every job whose cron expression matches the CURRENT
   minute in the job's declared timezone. `pp-cron run` runs one tick and
   exits; `pp-cron run --watch` ticks once a minute.

   Laws carried here, from pp-cron.borg sections scheduler and executor:

   . occurrence is the INTENDED fire - the minute the tick is serving -
     computed from the schedule, never from when the tick happened to run.
   . run_id is minted BEFORE the child starts (Spec.begin_run) and is the
     same for every attempt of one occurrence; only [attempt] differs, so a
     job's own idempotency key has one obvious value across retries.
   . the child runs in a fresh process group with a deadline, so the
     deadline kills what the job spawned too; stdout+stderr go to a spool
     file, never a pipe, and only the bounded tail enters the record.
   . the child's environment is the job's env file plus the RUN_ID
     contract; nothing else is inherited (minimal PATH and TZ only).
   . every attempt appends exactly one record through [Store.insert] - the
     single append path in the store. Nothing else writes the store.

   Deliberately not here yet (S3): SQLite, the outbox row in the same
   transaction, persisted running rows, and cross-occurrence overlap.
   Overlap is tracked in [in_flight] within this process only, so `skip is
   enforced between concurrent firings of one process and nothing more. *)

(* ------------------------------------------------------------------ *)
(* When a job is due                                                    *)
(* ------------------------------------------------------------------ *)

let sleep f = if f > 0. then ignore (Unix.select [] [] [] f)

(* The occurrence is the minute the tick serves, floored: a run that started
   40s late still names the occurrence it served. *)
let minute_floor t = Float.of_int (int_of_float (Float.floor (t /. 60.))) *. 60.

(* Each job declares its tz; UTC is the default the files must say out loud.
   A stub for now: localtime for anything but UTC, which is honest about
   DST being a per-job decision S3 makes. *)
let tm_in ~tz t =
  if String.uppercase_ascii tz = "UTC" then Unix.gmtime t else Unix.localtime t

let matches ~file ~(job : Spec.job) ~now =
  match Spec.parse_cron ~file ~line:1 job.schedule with
  | c -> Spec.cron_matches c (tm_in ~tz:job.tz now)
  | exception Spec.Refused _ -> false

(* ------------------------------------------------------------------ *)
(* The box identity: a stable id from install, NEVER a hostname         *)
(* ------------------------------------------------------------------ *)

let box_id () =
  match Sys.getenv_opt "PP_CRON_BOX" with
  | Some b when b <> "" -> b
  | _ -> (
    match Spec.read_file "/etc/pp-cron/box" with
    | Ok s when String.trim s <> "" -> String.trim s
    | _ -> "unset")

let rec ensure_dir path =
  if path = "" || path = "/" || path = "." || Sys.file_exists path then Ok ()
  else
    match ensure_dir (Filename.dirname path) with
    | Error e -> Error e
    | Ok () -> (
      try
        Unix.mkdir path 0o755;
        Ok ()
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
      | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e))

(* ------------------------------------------------------------------ *)
(* The child environment: the job's env file plus the RUN_ID contract    *)
(* ------------------------------------------------------------------ *)

let parse_env_file path =
  match Spec.read_file path with
  | Error m -> Error m
  | Ok src ->
    let err = ref None in
    let pairs =
      List.filter_map
        (fun line ->
           let line = String.trim line in
           if line = "" || line.[0] = '#' then None
           else
             match String.index_opt line '=' with
             | None ->
               if !err = None then
                 err := Some (Printf.sprintf "%s: %S is not KEY=VALUE" path line);
               None
             | Some i ->
               let k = String.trim (String.sub line 0 i) in
               let v = String.sub line (i + 1) (String.length line - i - 1) in
               if k = "" then begin
                 if !err = None then
                   err := Some (Printf.sprintf "%s: %S has an empty key" path line);
                 None
               end
               else Some (k, v))
        (String.split_on_char '\n' src)
    in
    (match !err with Some e -> Error e | None -> Ok pairs)

let contract ~box ~job ~occurrence ~attempt ~run_id =
  [ ("RUN_ID", run_id);
    ("PP_CRON_JOB", job);
    ("PP_CRON_OCCURRENCE", Printf.sprintf "%.0f" occurrence);
    ("PP_CRON_ATTEMPT", string_of_int attempt);
    ("PP_CRON_BOX", box) ]

(* Last binding wins, so the contract cannot be shadowed by a file entry. *)
let dedup pairs =
  List.fold_left
    (fun acc (k, v) -> (k, v) :: List.filter (fun (k', _) -> k' <> k) acc)
    [] pairs
  |> List.rev

let child_env ~box ~(job : Spec.job) ~occurrence ~attempt ~run_id =
  match
    match job.env_file with None -> Ok [] | Some p -> parse_env_file p
  with
  | Error e -> Error e
  | Ok kvs ->
    let path =
      match Sys.getenv_opt "PATH" with
      | Some p when p <> "" -> p
      | _ -> "/usr/bin:/bin"
    in
    let contract = contract ~box ~job:job.name ~occurrence ~attempt ~run_id in
    let reserved = List.map fst contract in
    let kvs = List.filter (fun (k, _) -> not (List.mem k reserved)) kvs in
    Ok (dedup ([ ("PATH", path); ("TZ", job.tz) ] @ kvs @ contract))

(* ------------------------------------------------------------------ *)
(* The child: fork, own process group, exec directly, spool the output   *)
(* ------------------------------------------------------------------ *)

let kill_group pid s =
  try Unix.kill (-pid) s with
  | Unix.Unix_error _ -> (try Unix.kill pid s with Unix.Unix_error _ -> ())

let rec wait_nohang pid =
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | 0, _ -> None
  | _, st -> Some st
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_nohang pid
  | exception Unix.Unix_error (_, _, _) -> Some (Unix.WEXITED 127)

let rec wait_until pid deadline =
  match wait_nohang pid with
  | Some st -> Some st
  | None ->
    if Unix.gettimeofday () >= deadline then None
    else begin
      sleep 0.02;
      wait_until pid deadline
    end

let spawn ~env ~spool (argv : string list) : (int, string) result =
  match argv with
  | [] -> Error "cmd: empty argv"
  | prog :: _ -> (
    match
      try Ok (Unix.fork ()) with Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
    with
    | Error e -> Error e
    | Ok 0 ->
      (* child: fresh process group, output to the spool file, no shell *)
      (try
         (try ignore (Unix.setsid ()) with Unix.Unix_error _ -> ());
         let fd =
           Unix.openfile spool [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
         in
         Unix.dup2 fd Unix.stdout;
         Unix.dup2 fd Unix.stderr;
         Unix.close fd;
         Unix.execve prog (Array.of_list argv) env
       with _ -> ());
      exit 127
    | Ok pid -> Ok pid)

(* SIGTERM the group, grace, SIGKILL the group, reap. *)
let exec_job ~timeout ~env ~spool argv =
  match spawn ~env ~spool argv with
  | Error e -> Error e
  | Ok pid -> (
    match wait_until pid (Unix.gettimeofday () +. timeout) with
    | Some st -> Ok (st, false)
    | None ->
      kill_group pid Sys.sigterm;
      (match wait_until pid (Unix.gettimeofday () +. 10.) with
       | Some st -> Ok (st, true)
       | None ->
         kill_group pid Sys.sigkill;
         (match wait_until pid (Unix.gettimeofday () +. 5.) with
          | Some st -> Ok (st, true)
          | None -> Error "child survived SIGKILL")))

let exit_and_signal (st : Unix.process_status) =
  match st with
  | Unix.WEXITED n -> (Some n, None)
  | Unix.WSIGNALED s -> (None, Some s)
  | Unix.WSTOPPED s -> (None, Some s)

let verdict_of_status ~timed_out (st : Unix.process_status) : Spec.box_verdict =
  if timed_out then `Timeout
  else
    match st with
    | Unix.WEXITED 0 -> `Ok
    | Unix.WEXITED n -> `Failed (Printf.sprintf "exit %d" n)
    | Unix.WSIGNALED s -> `Failed (Printf.sprintf "signal %d" s)
    | Unix.WSTOPPED s -> `Failed (Printf.sprintf "stopped %d" s)

let read_tail path =
  try
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Spec.bounded_log_tail s
  with Sys_error _ -> ""

(* ------------------------------------------------------------------ *)
(* One attempt: minted identity, child, record appended exactly once    *)
(* ------------------------------------------------------------------ *)

let attempt ~store ~box ~spool_dir ~(job : Spec.job) ~occurrence
    ~(start : Spec.run_start) =
  match child_env ~box ~job ~occurrence ~attempt:start.attempt
          ~run_id:start.run_id
  with
  | Error e -> Error e
  | Ok pairs ->
    let started = Unix.gettimeofday () in
    let start = { start with started } in
    let spool =
      Filename.concat spool_dir
        (Printf.sprintf "%s.%d.log" start.run_id start.attempt)
    in
    let env = Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) pairs) in
    (match exec_job ~timeout:job.timeout ~env ~spool job.cmd with
     | Error e ->
       let run =
         Spec.finish_run ~start (`Failed ("spawn: " ^ e))
           ~finished:(Unix.gettimeofday ()) ()
       in
       (match Store.insert store run with
        | Ok () -> Ok (run, false)
        | Error e -> Error e)
     | Ok (status, timed_out) ->
       let finished = Unix.gettimeofday () in
       let log_tail = read_tail spool in
       (try Sys.remove spool with Sys_error _ -> ());
       let exit, signal = exit_and_signal status in
       let run =
         Spec.finish_run ~start (verdict_of_status ~timed_out status) ~finished
           ?exit ?signal ~log_tail ()
       in
       (match Store.insert store run with
        | Ok () -> Ok (run, (not timed_out) && status = Unix.WEXITED 0)
        | Error e -> Error e))

(* Overlap policy. `skip is the default: an occurrence is not stacked on a
   predecessor still running. `queue and `kill degrade to sequential firing
   in this stub, which is one occurrence at a time by construction. *)
let in_flight : (string, float) Hashtbl.t = Hashtbl.create 17

let overlap_allows ~(job : Spec.job) =
  match job.overlap with
  | `Skip -> not (Hashtbl.mem in_flight job.name)
  | `Queue | `Kill _ -> true

(* One occurrence: attempt 1, then retries per the job's retry budget, same
   run_id throughout. Returns how many records were appended. *)
let fire ~store ~box ~spool_dir ~(job : Spec.job) ~occurrence =
  let start0 =
    Spec.begin_run ~box ~job:job.name ~occurrence ~now:(Unix.gettimeofday ()) ()
  in
  let attempts, delay =
    match job.retry with Some (a, d) -> (max 1 a, d) | None -> (1, 0.)
  in
  let appended = ref 0 in
  let rec go n =
    let start =
      { Spec.box = start0.box;
        run_id = start0.run_id;
        job = start0.job;
        occurrence = start0.occurrence;
        started = start0.started;
        attempt = n }
    in
    match attempt ~store ~box ~spool_dir ~job ~occurrence ~start with
    | Error e -> Error e
    | Ok (_run, ok) ->
      incr appended;
      if ok || n >= attempts then Ok ()
      else begin
        sleep delay;
        go (n + 1)
      end
  in
  match go 1 with Ok () -> Ok !appended | Error e -> Error e

(* ------------------------------------------------------------------ *)
(* One tick: pop due, fire, append. No match means the store is not     *)
(* even opened, so a quiet minute leaves it byte-for-byte unchanged.    *)
(* ------------------------------------------------------------------ *)

let tick ~store ~spool ~box ~now (jobs : Spec.job list) : (int, string) result =
  let due =
    List.filter
      (fun (job : Spec.job) ->
         matches ~file:("<job:" ^ job.name ^ ">") ~job ~now)
      jobs
  in
  match due with
  | [] -> Ok 0
  | due -> (
    match ensure_dir spool with
    | Error e -> Error e
    | Ok () -> (
      match Store.open_store ~path:store with
      | Error e -> Error e
      | Ok store ->
        let occurrence = minute_floor now in
        let fired = ref 0 and err = ref None in
        List.iter
          (fun (job : Spec.job) ->
             match !err with
             | Some _ -> ()
             | None ->
               if overlap_allows ~job then begin
                 Hashtbl.replace in_flight job.name occurrence;
                 (match
                    fire ~store ~box ~spool_dir:spool ~job ~occurrence
                  with
                  | Ok n -> fired := !fired + n
                  | Error e ->
                    err := Some (Printf.sprintf "%s: %s" job.name e));
                 Hashtbl.remove in_flight job.name
               end)
          due;
        match !err with Some e -> Error e | None -> Ok !fired))
