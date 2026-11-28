(* The scheduler's timing core: a min-heap of next fire times, occurrence
   arithmetic in the job's declared timezone, replay from persisted last-fire
   state, and the flock that keeps one daemon per box.

   Nothing here waits on a socket or talks to a database. Wall clock is used
   only to COMPUTE an occurrence; [replay] recomputes from the last fire the
   daemon persisted instead of trusting a sleep deadline, so an NTP step
   neither storms nor skips. *)

(* ------------------------------------------------------------------ *)
(* Min-heap keyed on a monotonic deadline, name breaking ties           *)
(* ------------------------------------------------------------------ *)

type t = { mutable items : (float * string) array; mutable n : int }

let create ?(capacity = 16) () =
  { items = Array.make (max 1 capacity) (0., ""); n = 0 }

let size t = t.n
let peek t = if t.n = 0 then None else Some t.items.(0)

let less (d1, n1) (d2, n2) = d1 < d2 || (d1 = d2 && String.compare n1 n2 < 0)

let ensure t =
  if t.n = Array.length t.items then begin
    let bigger = Array.make (2 * Array.length t.items) (0., "") in
    Array.blit t.items 0 bigger 0 t.n;
    t.items <- bigger
  end

let push t (due, name) =
  ensure t;
  let i = ref t.n in
  t.items.(!i) <- (due, name);
  t.n <- t.n + 1;
  while !i > 0 && less t.items.(!i) t.items.((!i - 1) / 2) do
    let parent = (!i - 1) / 2 in
    let tmp = t.items.(parent) in
    t.items.(parent) <- t.items.(!i);
    t.items.(!i) <- tmp;
    i := parent
  done

let pop t =
  if t.n = 0 then None
  else begin
    let top = t.items.(0) in
    t.n <- t.n - 1;
    t.items.(0) <- t.items.(t.n);
    let i = ref 0 and more = ref true in
    while !more do
      let l = (2 * !i) + 1 and r = (2 * !i) + 2 in
      let smallest = ref !i in
      if l < t.n && less t.items.(l) t.items.(!smallest) then smallest := l;
      if r < t.n && less t.items.(r) t.items.(!smallest) then smallest := r;
      if !smallest = !i then more := false
      else begin
        let tmp = t.items.(!i) in
        t.items.(!i) <- t.items.(!smallest);
        t.items.(!smallest) <- tmp;
        i := !smallest
      end
    done;
    Some top
  end

(* ------------------------------------------------------------------ *)
(* Timezones: libc's localtime with the job's TZ, restored afterwards   *)
(* ------------------------------------------------------------------ *)

let with_tz tz f =
  let saved = Sys.getenv_opt "TZ" in
  (match tz with Some z -> Unix.putenv "TZ" z | None -> ());
  let restore () =
    (* Unix has no unsetenv: an absent TZ comes back as UTC, which is the
       daemon's own clock. Every job names its tz explicitly, and localtime is
       only called inside this bracket. *)
    Unix.putenv "TZ" (Option.value saved ~default:"UTC")
  in
  match f () with
  | v ->
    restore ();
    v
  | exception e ->
    restore ();
    raise e

let local_tm tz t = with_tz (Some tz) (fun () -> Unix.localtime t)

(* Days since 1970-01-01, Howard Hinnant's civil calendar algorithm. *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let mp = (m + 9) mod 12 in
  let doy = ((153 * mp) + 2) / 5 + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  let y = if m <= 2 then y + 1 else y in
  (y, m, d)

(* 1970-01-01 was a Thursday (tm_wday = 4). *)
let weekday_of_days z = (((z + 4) mod 7) + 7) mod 7

(* The epoch for a local wall time, or None when it does not exist (a DST
   gap). A repeated local time (a DST overlap) fires once, at the earlier
   epoch. This is the per-job DST rule: the job's declared clock decides. *)
let epoch_of_local tz ~y ~m ~d ~hh ~mm =
  let attempt isdst =
    try
      let tm =
        { Unix.tm_sec = 0;
          tm_min = mm;
          tm_hour = hh;
          tm_mday = d;
          tm_mon = m - 1;
          tm_year = y - 1900;
          tm_wday = 0;
          tm_yday = 0;
          tm_isdst = isdst }
      in
      let t, _ = with_tz (Some tz) (fun () -> Unix.mktime tm) in
      let back = local_tm tz t in
      if back.Unix.tm_year + 1900 = y && back.Unix.tm_mon + 1 = m
         && back.Unix.tm_mday = d && back.Unix.tm_hour = hh
         && back.Unix.tm_min = mm
      then Some t
      else None
    with _ -> None
  in
  match (attempt false, attempt true) with
  | Some a, Some b -> Some (Float.min a b)
  | Some a, None -> Some a
  | None, Some b -> Some b
  | None, None -> None

let day_matches (c : Spec.cron) ~dom ~dow =
  let dom_ok = List.mem dom c.Spec.doms in
  let dow_ok = List.mem dow c.Spec.dows in
  if c.Spec.dom_restricted && c.Spec.dow_restricted then dom_ok || dow_ok
  else dom_ok && dow_ok

(* The next occurrence strictly after [after], computed on the job's clock. *)
let next_occurrence ?(tz = "UTC") ~(cron : Spec.cron) ~(after : float) () =
  let candidates =
    List.concat_map
      (fun hh -> List.map (fun mm -> (hh, mm)) cron.Spec.minutes)
      cron.Spec.hours
    |> List.sort compare
  in
  let start = local_tm tz (after +. 1.) in
  let day0 =
    days_from_civil (start.Unix.tm_year + 1900) (start.Unix.tm_mon + 1)
      start.Unix.tm_mday
  in
  let horizon_days = 366 * 5 in
  let rec over_days k =
    if k > horizon_days then None
    else
      let y, m, d = civil_from_days (day0 + k) in
      let dow = weekday_of_days (day0 + k) in
      if not (List.mem m cron.Spec.months && day_matches cron ~dom:d ~dow) then
        over_days (k + 1)
      else
        let rec over_times = function
          | [] -> over_days (k + 1)
          | (hh, mm) :: rest -> (
            match epoch_of_local tz ~y ~m ~d ~hh ~mm with
            | Some t when t > after -> Some t
            | _ -> over_times rest)
        in
        over_times candidates
  in
  over_days 0

let occurrences_between ?(tz = "UTC") ~(cron : Spec.cron) ~(from : float)
    ~(until : float) () =
  let rec go after count acc =
    if count >= 1000 then List.rev acc
    else
      match next_occurrence ~tz ~cron ~after () with
      | Some t when t <= until -> go t (count + 1) (t :: acc)
      | _ -> List.rev acc
  in
  go from 0 []

(* What the daemon owes after a restart or a wall-clock step: every occurrence
   after the last one it persisted, up to now. Recomputing from that state is
   the clock-step law. *)
let replay ?(tz = "UTC") ~(cron : Spec.cron) ~(last_fired : float) ~(now : float)
    () =
  occurrences_between ~tz ~cron ~from:last_fired ~until:now ()

(* ------------------------------------------------------------------ *)
(* One daemon per box: flock on the pidfile                             *)
(* ------------------------------------------------------------------ *)

type lock = { fd : Unix.file_descr; path : string }

let single_instance path =
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  try
    Unix.lockf fd Unix.F_TLOCK 0;
    Unix.ftruncate fd 0;
    let who = Printf.sprintf "%d\n" (Unix.getpid ()) in
    ignore (Unix.write_substring fd who 0 (String.length who));
    `Acquired { fd; path }
  with Unix.Unix_error (e, _, _)
    when e = Unix.EACCES || e = Unix.EAGAIN ->
    Unix.close fd;
    `Running

let release (l : lock) = Unix.close l.fd
