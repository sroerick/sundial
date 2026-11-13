(* pp-cron heartbeat client (card 32d76f, S3c).

   The sending half of the dead-man switch: every interval the daemon POSTs
   one liveness row to POST /api/cron/heartbeat, independent of any job.
   Central turns that silence into an incident, which is the only signal that
   catches a dead daemon (the gregor 38h case) - a dead daemon produces no job
   rows at all, and "no rows" is indistinguishable from "job removed".

   The clock and the sink are injected so the backoff law is testable without
   sleeping or touching the network. The sink is one function
   [path -> body -> status]: 2xx means delivered, anything else (including 0
   for a transport error) means blind retry later. *)

let default_interval = 60.
let default_ceiling = 300.
let heartbeat_path = "/api/cron/heartbeat"

type snapshot = {
  box : string;
  ts : float;
  uptime_s : float;
  jobs_loaded : int;
  pending_outbox : int;
  in_flight : int;
}

let json_of_snapshot s =
  Printf.sprintf
    "{\"box\":%S,\"ts\":%.3f,\"uptime_s\":%.3f,\"jobs_loaded\":%d,\
     \"pending_outbox\":%d,\"in_flight\":%d}"
    s.box s.ts s.uptime_s s.jobs_loaded s.pending_outbox s.in_flight

type sink = { post : path:string -> body:string -> int }

type t = {
  box : string;
  interval : float;          (* seconds between liveness POSTs while healthy *)
  backoff : bool;            (* exponential retry on 5xx and transport errors *)
  disabled : bool;           (* --no-heartbeat: a deliberately dark box *)
  ceiling : float;           (* retry gap ceiling, seconds *)
  clock : unit -> float;
  sink : sink;
  start : float;
  mutable jobs_loaded : int;
  mutable pending_outbox : int;
  mutable in_flight : int;
  mutable due : float;       (* earliest time the next POST may go out *)
  mutable failures : int;    (* consecutive failed POSTs *)
  mutable last_status : int; (* 0 = never posted *)
}

let create ?(interval = default_interval) ?(backoff = true) ?(disabled = false)
    ?(ceiling = default_ceiling) ~box ~clock ~sink () =
  let now = clock () in
  { box; interval; backoff; disabled; ceiling; clock; sink; start = now;
    jobs_loaded = 0; pending_outbox = 0; in_flight = 0; due = now;
    failures = 0; last_status = 0 }

let update t ~jobs_loaded ~pending_outbox ~in_flight =
  t.jobs_loaded <- jobs_loaded;
  t.pending_outbox <- pending_outbox;
  t.in_flight <- in_flight

let snapshot t =
  let now = t.clock () in
  { box = t.box; ts = now; uptime_s = now -. t.start; jobs_loaded = t.jobs_loaded;
    pending_outbox = t.pending_outbox; in_flight = t.in_flight }

(* Off: the loop's own tick is the cadence, so the next tick posts again.
   On:  one interval on success, doubling per consecutive failure up to the
   ceiling - never a tight retry loop, never a blocked firing. *)
let delay t =
  if not t.backoff then 0.
  else
    let n = if t.failures = 0 then 0 else t.failures - 1 in
    Float.min (t.interval *. (2. ** float_of_int n)) t.ceiling

let tick t =
  if t.disabled then ()
  else
    let now = t.clock () in
    if now < t.due then ()
    else begin
      let body = json_of_snapshot (snapshot t) in
      let status = t.sink.post ~path:heartbeat_path ~body in
      t.last_status <- status;
      (if status >= 200 && status < 300 then t.failures <- 0
       else t.failures <- t.failures + 1);
      t.due <- now +. delay t
    end

let ticks t n = for _ = 1 to n do tick t done
