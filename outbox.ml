(* pp-cron outbox shipper (card S3b, c724ca): the duplicate-ACK ship loop.

   Shipping is a separate loop in a separate failure domain: it drains the
   outbox table and does nothing else. Retries are blind because the receiver
   is idempotent (tuna's UNIQUE (box, run_id) plus upsert makes a duplicate
   post a no-op), so the loop can be dumb enough to be reliable:

     `Ack   -> delivered: exactly one send per record.
     `Dup   -> the receiver already has it: delivered, and NO retry.
     timeout -> exactly one retry, then `Error; the row stays queued.
     `Error -> not transport, not delivered: the row stays queued.

   The transport is one function [payload -> reply], so the laws above are
   counted by a fake transport in outbox_test without a socket. *)

(* One send plus exactly one retry. *)
let max_attempts = 2

type ack = [ `Ack | `Dup ]
type reply = [ `Ack of ack | `Timeout | `Error of string ]

type transport = { post : payload:string -> reply }

let ship t ~payload =
  let rec go attempt =
    match t.post ~payload with
    | `Ack ack -> Ok ack
    | `Error e -> Error (Printf.sprintf "sink error: %s" e)
    | `Timeout ->
      if attempt >= max_attempts then
        Error (Printf.sprintf "timed out after %d attempt(s)" attempt)
      else go (attempt + 1)
  in
  go 1

(* ------------------------------------------------------------------ *)
(* The queue file: one payload per line, oldest first. A row leaves the *)
(* file only when it was delivered (ack or duplicate ack).              *)
(* ------------------------------------------------------------------ *)

let read_queue path =
  try
    let ic = open_in_bin path in
    let rec loop acc =
      match input_line ic with
      | line -> loop (line :: acc)
      | exception End_of_file ->
        close_in ic;
        Ok (List.rev acc)
    in
    loop []
  with Sys_error m -> Error m

let write_queue path payloads =
  try
    let oc = open_out_bin path in
    List.iter
      (fun p ->
         output_string oc p;
         output_char oc '\n')
      payloads;
    flush oc;
    close_out oc;
    Ok ()
  with Sys_error m -> Error m

(* One pass, oldest first. On the first row that could not be shipped the
   pass stops and the queue is rewritten with that row and everything behind
   it, so ordering is preserved for the next pass. *)
let drain_file t ~path =
  let rec go = function
    | [] -> Ok ()
    | p :: rest -> (
      match ship t ~payload:p with
      | Ok _ -> go rest
      | Error e -> Error (e, p :: rest))
  in
  match read_queue path with
  | Error m -> Error m
  | Ok [] -> Ok 0
  | Ok payloads -> (
    match go payloads with
    | Error (e, pending) -> (
      match write_queue path pending with
      | Ok () -> Error e
      | Error m -> Error m)
    | Ok _ -> (
      match write_queue path [] with
      | Ok () -> Ok (List.length payloads)
      | Error m -> Error m))
