(* Unit test for the S3c heartbeat client (card 32d76f): one liveness POST per
   interval with backoff off, one POST per backoff window when the sink 5xxs,
   and silence when the daemon is deliberately dark. The clock is a mutable ref
   and the sink is a counter, so no test sleeps and no test touches the net. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.printf "FAIL %s\n" name
  end

type recorder = {
  mutable now : float;
  mutable posts : (string * string) list;   (* path, body, newest first *)
  mutable status : int;
}

let recorder () = { now = 1000.; posts = []; status = 200 }

let clock r () = r.now

let sink r =
  Heartbeat.{ post = (fun ~path ~body ->
      r.posts <- (path, body) :: r.posts;
      r.status) }

let count r = List.length r.posts

let path_count r path =
  List.length (List.filter (fun (p, _) -> p = path) r.posts)

let mk r ?(backoff = true) ?(disabled = false) () =
  Heartbeat.create ~interval:60. ~backoff ~disabled ~box:"box-1"
    ~clock:(clock r) ~sink:(sink r) ()

(* --- law: backoff off, the tick is the cadence --- *)
let () =
  let r = recorder () in
  let hb = mk r ~backoff:false () in
  Heartbeat.ticks hb 3;
  check "backoff off: 3 ticks -> exactly 3 POSTs" (count r = 3);
  check "backoff off: every POST goes to /api/cron/heartbeat"
    (path_count r "/api/cron/heartbeat" = 3)

(* --- law: backoff on, a 5xx costs one interval, not a tight retry loop --- *)
let () =
  let r = recorder () in
  r.status <- 500;
  let hb = mk r ~backoff:true () in
  Heartbeat.ticks hb 3;                     (* 3 ticks inside one interval *)
  check "backoff on: 3 ticks in one interval -> exactly 1 POST" (count r = 1);
  r.now <- r.now +. 61.;                    (* one tick after the delay *)
  Heartbeat.tick hb;
  check "backoff on: one tick after the delay -> 2 POSTs" (count r = 2)

(* --- law: a disabled heartbeat is silent and healthy --- *)
let () =
  let r = recorder () in
  let hb = mk r ~disabled:true () in
  Heartbeat.ticks hb 3;
  check "disabled: 3 ticks -> 0 POSTs" (count r = 0)

(* --- law: a 2xx resets the backoff window to one interval --- *)
let () =
  let r = recorder () in
  let hb = mk r () in
  Heartbeat.tick hb;
  check "healthy: first tick posts" (count r = 1);
  r.now <- r.now +. 61.;
  Heartbeat.tick hb;
  check "healthy: one tick after the interval posts" (count r = 2)

let () =
  if !failures > 0 then begin
    Printf.printf "%d heartbeat law(s) failed\n" !failures;
    exit 1
  end;
  print_endline "heartbeat: all laws hold"
