(* The shipper's policy, not its socket: batch selection, backoff, dead-letter.

   Blind retries are safe because the receiver is idempotent; this module only
   decides what one row does after one attempt, so the loop stays trivial. *)

type row = {
  run_id : string;
  payload : string;
  created_at : float;
  attempts : int;
}

type outcome =
  [ `Ok
  | `Client_error of int
  | `Server_error of int
  | `Transport of string ]

type action = [ `Delete | `Retry_after of float | `Dead_letter of string ]

let batch_size = 200
let backoff_ceiling = 300.
let client_error_limit = 3

(* Oldest first, run_id breaking ties so a batch is reproducible. *)
let batch ?(size = batch_size) (rows : row list) : row list =
  let sorted =
    List.sort
      (fun a b ->
        match Float.compare a.created_at b.created_at with
        | 0 -> String.compare a.run_id b.run_id
        | c -> c)
      rows
  in
  let rec take n = function
    | [] -> []
    | _ when n <= 0 -> []
    | x :: rest -> x :: take (n - 1) rest
  in
  take size sorted

(* attempts is 1-based: the first failed attempt waits 1s, doubling to 5m. *)
let base_delay attempts =
  let rec go n d =
    if n <= 1 then d else go (n - 1) (Float.min backoff_ceiling (d *. 2.))
  in
  go attempts 1.

(* Full-ish jitter: half the delay up to the delay, never past the ceiling. *)
let jitter ~(rand : unit -> float) base = base *. (0.5 +. (rand () *. 0.5))

let backoff ~(rand : unit -> float) ~attempts = jitter ~rand (base_delay attempts)

let decide ~(rand : unit -> float) (row : row) (outcome : outcome) : action =
  match outcome with
  | `Ok -> `Delete
  | `Client_error code ->
    let attempts = row.attempts + 1 in
    if attempts >= client_error_limit then
      `Dead_letter (Printf.sprintf "http %d after %d attempts" code attempts)
    else `Retry_after (backoff ~rand ~attempts)
  | `Server_error _ | `Transport _ ->
    `Retry_after (backoff ~rand ~attempts:(row.attempts + 1))

let backpressure_threshold = 10_000

let backpressured ?(threshold = backpressure_threshold) ~(pending : int) () =
  pending > threshold
