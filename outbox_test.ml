(* card S3b: the duplicate-ACK ship loop. A fake transport records every
   send, so the laws are counted rather than assumed: one send per record
   under an ack, zero retries on a duplicate ack, exactly one retry on a
   timeout. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    prerr_endline ("FAIL: " ^ name)
  end

(* [fake script] answers sends in order and counts them. *)
let fake script =
  let sends = ref 0 in
  let script = ref script in
  let post ~payload =
    ignore payload;
    incr sends;
    match !script with
    | r :: rest ->
      script := rest;
      r
    | [] -> `Error "unexpected extra send"
  in
  ({ Outbox.post = post }, sends)

let () =
  (* an ack: exactly one send per record *)
  let t, sends = fake [ `Ack `Ack ] in
  (match Outbox.ship t ~payload:"a" with
   | Ok `Ack -> ()
   | Ok `Dup | Error _ -> check "ack: delivered" false);
  check "ack: exactly one send" (!sends = 1);

  (* a duplicate ack: the receiver already has it, so no retry at all *)
  let t, sends = fake [ `Ack `Dup ] in
  (match Outbox.ship t ~payload:"b" with
   | Ok `Dup -> ()
   | Ok `Ack | Error _ -> check "dup: reported as duplicate" false);
  check "dup: exactly one send, zero retries" (!sends = 1);

  (* a timeout: exactly one retry, then delivered *)
  let t, sends = fake [ `Timeout; `Ack `Ack ] in
  (match Outbox.ship t ~payload:"c" with
   | Ok `Ack -> ()
   | Ok `Dup | Error _ -> check "timeout: retried and delivered" false);
  check "timeout: exactly one retry" (!sends = 2);

  (* two timeouts: the retry is spent, the record is reported, not dropped *)
  let t, sends = fake [ `Timeout; `Timeout ] in
  (match Outbox.ship t ~payload:"d" with
   | Error _ -> ()
   | Ok _ -> check "timeout twice: reported failed" false);
  check "timeout twice: one retry only" (!sends = 2);

  (* the queue: one send per queued record, delivered rows leave the file *)
  let path = Filename.temp_file "outbox" "queue" in
  let oc = open_out_bin path in
  output_string oc "r1\nr2\n";
  close_out oc;
  let t, sends = fake [ `Ack `Ack; `Ack `Dup ] in
  (match Outbox.drain_file t ~path with
   | Ok 2 -> ()
   | Ok _ | Error _ -> check "drain: two records shipped" false);
  check "drain: exactly one send per record" (!sends = 2);
  let ic = open_in_bin path in
  let empty = in_channel_length ic = 0 in
  close_in ic;
  check "drain: delivered rows left the queue" empty;
  Sys.remove path;

  if !failures = 0 then print_endline "outbox: ok"
  else begin
    Printf.printf "outbox: %d failure(s)\n" !failures;
    exit 1
  end
