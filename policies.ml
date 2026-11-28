(* pp-cron policies: overlap, catch-up and retry as pure decisions.

   The caller owns the clock, the process table and the store. This module
   only says what should happen, so each rule is provable without a daemon. *)

type overlap_action = [ `Dispatch | `Skip_recorded | `Queue | `Kill_after of float ]

(* [predecessor] is the run of the same job that would overlap this firing. *)
let overlap_action (policy : Spec.overlap)
    (predecessor : [ `Done | `Running of float ]) =
  match predecessor with
  | `Done -> `Dispatch
  | `Running _ -> (
    match policy with
    | `Skip -> `Skip_recorded
    | `Queue -> `Queue
    | `Kill grace -> `Kill_after grace)

(* A skipped firing is recorded, not silently dropped: neither verdict is a
   success, and neither run disappears. *)
let skipped_verdict : Spec.box_verdict = `Failed "overlap-skip"
let killed_verdict : Spec.box_verdict = `Failed "overlap-kill"

(* Firings whose time elapsed while the box or the daemon was down,
   oldest-first. A catch-up run keeps the occurrence it serves. *)
let catch_up (policy : Spec.on_miss) ~(missed : float list) =
  match policy with
  | `Skip -> []
  | `Once -> ( match List.rev missed with [] -> [] | latest :: _ -> [ latest ])
  | `All -> missed

type retry_action = [ `Final | `Retry_after of float ]

(* [retry] is (total attempts within one occurrence, delay seconds); attempts
   are 1-based, so attempt 1 of (2, 300) still has one retry left. *)
let retry_action ~(retry : (int * float) option) ~(attempt : int) =
  match retry with
  | None -> `Final
  | Some (attempts, delay) ->
    if attempt < attempts then `Retry_after delay else `Final

(* Retry covers the process failing; an [Ok] verdict ends the occurrence. *)
let after_verdict ~(retry : (int * float) option) ~(attempt : int)
    (verdict : Spec.box_verdict) =
  match verdict with
  | `Ok -> `Final
  | `Failed _ | `Timeout -> retry_action ~retry ~attempt

let exit_is_ok ~(ok_codes : int list) (exit : int) =
  exit = 0 || List.mem exit ok_codes

(* A non-zero exit is a failure unless the job declares ok-codes. *)
let verdict_of_exit ~(ok_codes : int list) (exit : int) : Spec.box_verdict =
  if exit_is_ok ~ok_codes exit then `Ok
  else `Failed (Printf.sprintf "exit %d" exit)
