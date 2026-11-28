(* The dead-man switch: a box heartbeat, and absence synthesis that keeps "the
   box was dead" apart from "this job was dead". *)

type beat = {
  box : string;
  ts : float;
  uptime_s : float;
  jobs_loaded : int;
  pending_outbox : int;
  in_flight : int;
}

type liveness = [ `Alive | `Gap ]

let interval = 60.
let gap_factor = 3.

(* Central opens an incident when the last beat is older than 3x the box's
   declared interval; a dead daemon produces no job rows at all. *)
let liveness ~(last_beat : float option) ~(now : float) =
  match last_beat with
  | None -> `Gap
  | Some ts -> if now -. ts > gap_factor *. interval then `Gap else `Alive

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let payload (b : beat) =
  Printf.sprintf
    {|{"box":"%s","ts":%.0f,"uptime_s":%.0f,"jobs_loaded":%d,"pending_outbox":%d,"in_flight":%d}|}
    (escape b.box) b.ts b.uptime_s b.jobs_loaded b.pending_outbox b.in_flight

(* A job central must reason about without observing it. *)
type expectation = {
  job : string;
  cron : Spec.cron;
  tz : string;
  heartbeat : float;
}

(* Occurrences in (since, now - heartbeat] that were never observed, and only
   where a beat shows the box alive for that window: a dead box gets one
   incident about the daemon, never four hundred missing-run rows. *)
let missing ~(since : float) ~(now : float)
    ~(expectations : expectation list)
    ~(observed : (string * float) list) ~(beats : float list) =
  List.concat_map
    (fun e ->
      let until = now -. e.heartbeat in
      if until <= since then []
      else
        let expected =
            Sched.occurrences_between ~tz:e.tz ~cron:e.cron ~from:since ~until ()
        in
        List.filter_map
          (fun occurrence ->
            let seen =
              List.exists
                (fun (job, t) ->
                  job = e.job && Float.abs (t -. occurrence) < 1.)
                observed
            in
            let covered =
              List.exists
                (fun ts -> ts >= occurrence && ts <= occurrence +. e.heartbeat)
                beats
            in
            if seen || not covered then None else Some (e.job, occurrence))
          expected)
    expectations
