(* pp-cron: the record, the schedule grammar, and the validation laws.

   The record is the product. This module is the single definition from
   which the local schema, the wire JSON and the central table are
   derived. Scope law (card a88d99, S1): record types + schedule grammar +
   validation + the check subcommand only - no scheduler, no executor, no
   store. *)

exception Refused of string

(* ------------------------------------------------------------------ *)
(* The record                                                          *)
(* ------------------------------------------------------------------ *)

type overlap = [ `Skip | `Queue | `Kill of float ]
type on_miss = [ `Skip | `Once | `All ]

(* `Missing is a CENTRAL-side verdict. A box never sends it: the box's own
   verdict type is [box_verdict], which has no such constructor, so the
   absence of a record can never be confused with a record saying there
   was no work. *)
type verdict = [ `Ok | `Failed of string | `Timeout | `Missing ]
type box_verdict = [ `Ok | `Failed of string | `Timeout ]

type job = {
  name : string;
  schedule : string;              (* cron expression *)
  tz : string;                    (* explicit; never an accident of localtime *)
  cmd : string list;
  env_file : string option;       (* local, mode 0600 daemon-owned *)
  timeout : float;                (* seconds *)
  overlap : overlap;
  on_miss : on_miss;
  retry : (int * float) option;   (* attempts, delay seconds *)
  heartbeat : float;              (* expected max gap, seconds *)
  tags : string list;
}

(* The identity of a firing, minted before the child is started. *)
type run_start = {
  box : string;                   (* stable id from install, NEVER a hostname *)
  run_id : string;                (* ULID, minted on the box *)
  job : string;
  occurrence : float;             (* INTENDED wall-clock fire, unix epoch *)
  started : float;
  attempt : int;                  (* 1-based within an occurrence *)
}

type run = {
  box : string;
  run_id : string;
  job : string;
  occurrence : float;
  started : float;
  finished : float option;
  exit : int option;
  signal : int option;
  verdict : verdict;
  attempt : int;
  log_tail : string;              (* bounded, 8 KiB *)
}

let to_verdict (v : box_verdict) : verdict =
  match v with `Ok -> `Ok | `Failed r -> `Failed r | `Timeout -> `Timeout

let verdict_to_wire = function
  | `Ok -> "ok"
  | `Failed _ -> "failed"
  | `Timeout -> "timeout"
  | `Missing -> "missing"

let log_tail_cap = 8192

let bounded_log_tail s =
  let n = String.length s in
  if n <= log_tail_cap then s else String.sub s (n - log_tail_cap) log_tail_cap

(* ------------------------------------------------------------------ *)
(* run_id: a ULID minted on the box before exec                        *)
(* ------------------------------------------------------------------ *)

let crockford = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

let ulid ~now rand =
  let ms = Int64.of_float (now *. 1000.) in
  let b = Bytes.make 26 '0' in
  let rec enc i v =
    if i >= 0 then begin
      Bytes.set b i crockford.[Int64.to_int (Int64.logand v 31L)];
      enc (i - 1) (Int64.shift_right_logical v 5)
    end
  in
  enc 9 ms;
  for i = 10 to 25 do
    Bytes.set b i crockford.[rand ()]
  done;
  Bytes.to_string b

let mint_run_id ~now =
  let seed = int_of_float (now *. 1000.) lxor Unix.getpid () in
  let st = Random.State.make [| seed; Random.bits () |] in
  ulid ~now (fun () -> Random.State.int st 32)

(* Called BEFORE fork/exec: the id exists even if the daemon dies mid-run. *)
let begin_run ~box ~job ~occurrence ~now ?(attempt = 1) () =
  { box; run_id = mint_run_id ~now; job; occurrence; started = now; attempt }

(* Called at reap, with a box-side verdict. Same run_id across retries of
   one occurrence; only [attempt] differs. *)
let finish_run ~(start : run_start) (v : box_verdict) ~finished ?exit ?signal
    ?(log_tail = "") () =
  { box = start.box;
    run_id = start.run_id;
    job = start.job;
    occurrence = start.occurrence;
    started = start.started;
    finished = Some finished;
    exit;
    signal;
    verdict = to_verdict v;
    attempt = start.attempt;
    log_tail = bounded_log_tail log_tail }

(* ------------------------------------------------------------------ *)
(* Refusal: every error names file:line                                *)
(* ------------------------------------------------------------------ *)

let refuse file line fmt =
  Printf.ksprintf
    (fun msg -> raise (Refused (Printf.sprintf "%s:%d: %s" file line msg)))
    fmt

(* ------------------------------------------------------------------ *)
(* S-expressions, with line numbers                                    *)
(* ------------------------------------------------------------------ *)

type sexp = Atom of string * int | List of sexp list * int

let line_of = function Atom (_, l) -> l | List (_, l) -> l

let parse_sexp file (src : string) : sexp list =
  let n = String.length src in
  let i = ref 0 and line = ref 1 in
  let peek () = if !i < n then Some src.[!i] else None in
  let bump () =
    (match peek () with Some '\n' -> incr line | _ -> ());
    incr i
  in
  let rec skip () =
    match peek () with
    | Some (' ' | '\t' | '\r' | '\n') -> bump (); skip ()
    | Some ';' ->
      while peek () <> None && peek () <> Some '\n' do bump () done;
      skip ()
    | _ -> ()
  in
  let at_delim c =
    match c with ' ' | '\t' | '\r' | '\n' | '(' | ')' | ';' | '"' -> true | _ -> false
  in
  let parse_string () =
    let start = !line in
    let b = Buffer.create 16 in
    bump ();
    let fin = ref false in
    while not !fin do
      match peek () with
      | None -> refuse file start "unterminated string"
      | Some '"' -> bump (); fin := true
      | Some '\\' -> (
        bump ();
        match peek () with
        | None -> refuse file start "unterminated escape in string"
        | Some c ->
          Buffer.add_char b (match c with 'n' -> '\n' | 't' -> '\t' | 'r' -> '\r' | c -> c);
          bump ())
      | Some c -> Buffer.add_char b c; bump ()
    done;
    Atom (Buffer.contents b, start)
  in
  let parse_atom () =
    let l = !line in
    let b = Buffer.create 16 in
    let fin = ref false in
    while not !fin do
      match peek () with
      | Some c when not (at_delim c) -> Buffer.add_char b c; bump ()
      | _ -> fin := true
    done;
    Atom (Buffer.contents b, l)
  in
  let rec parse_one () =
    skip ();
    match peek () with
    | None -> refuse file !line "unexpected end of file"
    | Some '(' ->
      let l = !line in
      bump ();
      let items = ref [] in
      let fin = ref false in
      while not !fin do
        skip ();
        match peek () with
        | None -> refuse file l "unclosed '('"
        | Some ')' -> bump (); fin := true
        | _ -> items := parse_one () :: !items
      done;
      List (List.rev !items, l)
    | Some ')' -> refuse file !line "unexpected ')'"
    | Some '"' -> parse_string ()
    | Some _ -> parse_atom ()
  in
  let out = ref [] in
  let fin = ref false in
  while not !fin do
    skip ();
    match peek () with None -> fin := true | Some _ -> out := parse_one () :: !out
  done;
  List.rev !out

let atom_of ~file = function
  | Atom (s, _) -> s
  | List (_, l) -> refuse file l "expected an atom"

(* ------------------------------------------------------------------ *)
(* The cron grammar, and the cadence it implies                        *)
(* ------------------------------------------------------------------ *)

type cron = {
  minutes : int list;
  hours : int list;
  doms : int list;
  months : int list;
  dows : int list;
  dom_restricted : bool;
  dow_restricted : bool;
}

let int_or_refuse ~file ~line ~name s =
  match int_of_string_opt s with
  | Some n -> n
  | None -> refuse file line "%s: expected a number, got %S" name s

let parse_field ~file ~line ~name ~lo ~hi s =
  let parts = String.split_on_char ',' s in
  let rec expand parts acc =
    match parts with
    | [] -> List.rev acc
    | part :: rest ->
      let base, step =
        match String.index_opt part '/' with
        | None -> (part, 1)
        | Some k ->
          let st =
            int_or_refuse ~file ~line ~name
              (String.sub part (k + 1) (String.length part - k - 1))
          in
          if st <= 0 then refuse file line "%s: step must be positive" name;
          (String.sub part 0 k, st)
      in
      let a, b =
        if base = "*" then (lo, hi)
        else
          match String.index_opt base '-' with
          | Some k ->
            ( int_or_refuse ~file ~line ~name (String.sub base 0 k),
              int_or_refuse ~file ~line ~name
                (String.sub base (k + 1) (String.length base - k - 1)) )
          | None ->
            let v = int_or_refuse ~file ~line ~name base in
            (v, v)
      in
      if a < lo || b > hi || a > b then
        refuse file line "%s: %S is outside %d-%d" name base lo hi;
      let rec go v acc = if v > b then List.rev acc else go (v + step) (v :: acc) in
      expand rest (List.rev_append (go a []) acc)
  in
  expand parts []

let parse_cron ~file ~line s =
  let fields =
    String.split_on_char ' ' (String.map (fun c -> if c = '\t' then ' ' else c) s)
    |> List.filter (fun f -> f <> "")
  in
  match fields with
  | [ mi; ho; dom; mon; dow ] ->
    { minutes = parse_field ~file ~line ~name:"minute" ~lo:0 ~hi:59 mi;
      hours = parse_field ~file ~line ~name:"hour" ~lo:0 ~hi:23 ho;
      doms = parse_field ~file ~line ~name:"day-of-month" ~lo:1 ~hi:31 dom;
      months = parse_field ~file ~line ~name:"month" ~lo:1 ~hi:12 mon;
      dows =
        List.map (fun d -> if d = 7 then 0 else d)
          (parse_field ~file ~line ~name:"day-of-week" ~lo:0 ~hi:7 dow);
      dom_restricted = dom <> "*";
      dow_restricted = dow <> "*" }
  | _ ->
    refuse file line
      "schedule %S: expected five cron fields (minute hour day month weekday)" s

let cron_matches c (t : Unix.tm) =
  List.mem t.tm_min c.minutes
  && List.mem t.tm_hour c.hours
  && List.mem (t.tm_mon + 1) c.months
  && (let dom_ok = List.mem t.tm_mday c.doms in
      let dow_ok = List.mem t.tm_wday c.dows in
      if c.dom_restricted && c.dow_restricted then dom_ok || dow_ok
      else dom_ok && dow_ok)

(* Smallest gap, in seconds, between two successive firings of the
   schedule: the cadence the declared heartbeat must cover. *)
let cadence_seconds c =
  let horizon = 6_000_000 (* minutes, ~11 years *) in
  let rec go m last =
    if m > horizon then None
    else if cron_matches c (Unix.gmtime (float_of_int (m * 60))) then
      match last with None -> go (m + 1) (Some m) | Some p -> Some ((m - p) * 60)
    else go (m + 1) last
  in
  go 0 None

(* ------------------------------------------------------------------ *)
(* env-from: KEY=VALUE, mode 0600, daemon-owned, or the job is refused  *)
(* ------------------------------------------------------------------ *)

let check_env_file ~file ~line p =
  let st =
    try Unix.stat p
    with Unix.Unix_error (e, _, _) ->
      refuse file line "env-from %s: %s" p (Unix.error_message e)
  in
  let mode = st.Unix.st_perm land 0o777 in
  if mode <> 0o600 then
    refuse file line "env-from %s: mode %04o, must be 0600 and daemon-owned" p mode;
  if st.Unix.st_uid <> Unix.geteuid () then
    refuse file line "env-from %s: owned by uid %d, must be the daemon user (uid %d)"
      p st.Unix.st_uid (Unix.geteuid ())

(* ------------------------------------------------------------------ *)
(* One schedule file: (job "name" ...) - unknown keys and values refused *)
(* ------------------------------------------------------------------ *)

let parse_job ~file (s : sexp) : job =
  let job_line, name, fields =
    match s with
    | List (Atom ("job", _) :: Atom (name, _) :: rest, l) -> (l, name, rest)
    | List (_, l) -> refuse file l "expected (job \"name\" ...)"
    | Atom (_, l) -> refuse file l "expected (job \"name\" ...)"
  in
  let seen = Hashtbl.create 17 in
  let sched = ref None and tz = ref None and cmd = ref None and env = ref None in
  let timeout = ref None and overlap = ref None and on_miss = ref None in
  let retry = ref None and heartbeat = ref None and tags = ref [] in
  let hb_line = ref job_line in
  List.iter
    (fun form ->
       match form with
       | List (Atom (k, kl) :: args, _) ->
         if Hashtbl.mem seen k then refuse file kl "duplicate key %S" k;
         Hashtbl.add seen k ();
         let one () =
           match args with
           | [ Atom (v, _) ] -> v
           | _ -> refuse file kl "%s: expected exactly one value" k
         in
         let num () =
           let v = one () in
           match float_of_string_opt v with
           | Some x -> x
           | None -> refuse file kl "%s: expected a number, got %S" k v
         in
         (match k with
          | "schedule" -> sched := Some (one (), kl)
          | "tz" -> tz := Some (one ())
          | "cmd" -> cmd := Some (List.map (atom_of ~file) args)
          | "env-from" -> env := Some (one ())
          | "timeout" -> timeout := Some (num ())
          | "overlap" ->
            overlap :=
              Some
                (match args with
                 | [ Atom ("skip", _) ] -> `Skip
                 | [ Atom ("queue", _) ] -> `Queue
                 | [ Atom ("kill", _); Atom (v, vl) ] -> (
                   match float_of_string_opt v with
                   | Some secs -> `Kill secs
                   | None -> refuse file vl "overlap kill: expected seconds")
                 | _ ->
                   refuse file kl "overlap: expected skip | queue | kill <seconds>")
          | "on-miss" ->
            on_miss :=
              Some
                (match args with
                 | [ Atom ("skip", _) ] -> `Skip
                 | [ Atom ("once", _) ] -> `Once
                 | [ Atom ("all", _) ] -> `All
                 | _ -> refuse file kl "on-miss: expected skip | once | all")
          | "retry" -> (
            match args with
            | [ Atom (a, al); Atom (d, _) ] ->
              let attempts =
                match int_of_string_opt a with
                | Some n when n >= 1 -> n
                | _ -> refuse file al "retry: attempts must be an integer >= 1"
              in
              let delay =
                match float_of_string_opt d with
                | Some x when x >= 0. -> x
                | _ -> refuse file al "retry: delay must be >= 0 seconds"
              in
              retry := Some (attempts, delay)
            | _ -> refuse file kl "retry: expected attempts and delay seconds")
          | "heartbeat" ->
            let v = num () in
            hb_line := kl;
            heartbeat := Some v
          | "tags" -> (
            match args with
            | [ List (items, _) ] -> tags := List.map (atom_of ~file) items
            | _ -> refuse file kl "tags: expected a list of strings")
          | _ -> refuse file kl "unknown key %S (refused, never ignored)" k)
       | List ([], l) -> refuse file l "empty form"
       | List (List (_, _) :: _, l) ->
         refuse file l "expected (key value), got a form whose head is not a key"
       | Atom (a, l) -> refuse file l "expected (key value), got %S" a)
    fields;
  let need key r =
    match !r with
    | Some v -> v
    | None -> refuse file job_line "missing required key %S" key
  in
  let schedule, sched_line = need "schedule" sched in
  let tz = need "tz" tz in
  let cmd = need "cmd" cmd in
  let timeout = need "timeout" timeout in
  let heartbeat = need "heartbeat" heartbeat in
  if cmd = [] then refuse file job_line "cmd: expected at least one argument";
  if timeout <= 0. then refuse file job_line "timeout: must be > 0 seconds";
  if heartbeat <= 0. then refuse file job_line "heartbeat: must be > 0 seconds";
  (match cadence_seconds (parse_cron ~file ~line:sched_line schedule) with
   | Some cadence when heartbeat < float_of_int cadence ->
     refuse file !hb_line
       "heartbeat %gs is smaller than the schedule cadence %ds for schedule %S (central would synthesize false missing verdicts)"
       heartbeat cadence schedule
   | _ -> ());
  (match !env with Some p -> check_env_file ~file ~line:job_line p | None -> ());
  { name;
    schedule;
    tz;
    cmd;
    env_file = !env;
    timeout;
    overlap = (match !overlap with Some o -> o | None -> `Skip);
    on_miss = (match !on_miss with Some m -> m | None -> `Skip);
    retry = !retry;
    heartbeat;
    tags = !tags }

(* ------------------------------------------------------------------ *)
(* The jobs directory: check, then fail-closed load                    *)
(* ------------------------------------------------------------------ *)

let read_file file =
  try
    let ic = open_in_bin file in
    let n = in_channel_length ic in
    let src = really_input_string ic n in
    close_in ic;
    Ok src
  with Sys_error m -> Error m

let job_of_file file : (job, string) result =
  match read_file file with
  | Error m -> Error (Printf.sprintf "%s:1: %s" file m)
  | Ok src -> (
    match (try Ok (parse_sexp file src) with Refused m -> Error m) with
    | Error m -> Error m
    | Ok [] -> Error (Printf.sprintf "%s:1: empty schedule file" file)
    | Ok [ one ] -> ( try Ok (parse_job ~file one) with Refused m -> Error m)
    | Ok (_ :: second :: _) ->
      Error
        (Printf.sprintf "%s:%d: expected exactly one (job ...) form" file
           (line_of second)))

let check_dir dir : (job list, string list) result =
  match (try Ok (Sys.readdir dir) with Sys_error m -> Error m) with
  | Error m -> Error [ Printf.sprintf "%s:1: %s" dir m ]
  | Ok entries ->
    let names =
      Array.to_list entries
      |> List.filter (fun n -> Filename.check_suffix n ".sexp")
      |> List.sort compare
    in
    let jobs = ref [] and errs = ref [] in
    List.iter
      (fun name ->
         match job_of_file (Filename.concat dir name) with
         | Ok j -> jobs := j :: !jobs
         | Error e -> errs := e :: !errs)
      names;
    let errs = List.rev !errs in
    if errs <> [] then Error errs
    else
      let sorted = List.sort (fun a b -> compare a.name b.name) (List.rev !jobs) in
      let rec dups = function
        | a :: (b :: _ as tl) ->
          if a.name = b.name then
            Printf.sprintf "%s:1: duplicate job name %S across schedule files" dir
              a.name
            :: dups tl
          else dups tl
        | _ -> []
      in
      (match dups sorted with [] -> Ok sorted | ds -> Error ds)

(* Fail closed on config: a bad file keeps the PREVIOUS known-good job set
   running, never an empty set. The refusal is logged naming file:line. *)
let load ?(prev = []) ?(log = fun m -> prerr_endline ("pp-cron: " ^ m)) dir =
  match check_dir dir with
  | Ok jobs -> jobs
  | Error errs ->
    List.iter log errs;
    prev
