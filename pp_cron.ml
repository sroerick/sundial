(* pp-cron command line. S1 (card a88d99) ships `check`: the record, the
   schedule grammar and its validation. S3a (card d1b6ba) links the
   durable run-record store (store.ml), exercised by store_test. S3b (card
   c724ca) ships `ship`: one pass of the outbox drain loop in outbox.ml -
   one send per queued record under an ack, no resend on a duplicate ack,
   exactly one retry on a timeout, and a row leaves the queue only when it
   was delivered. *)

let usage =
  "usage: pp-cron check [<jobs-dir>...]\n\
  \  pp-cron run [--jobs <dir>] [--store <file>] [--watch]\n\
  \  pp-cron ship [<queue-file>]\n\
  \  (default jobs directory: /etc/pp-cron/jobs)\n\
  \  (default store: $PP_CRON_STORE or /var/db/pp-cron/runs.jsonl)\n\
  \  (default queue: $PP_CRON_OUTBOX or /var/db/pp-cron/outbox)\n\
  \  (sink: $PP_CRON_SINK or http://127.0.0.1:8080/api/cron/runs)"

let default_sink = "http://127.0.0.1:8080/api/cron/runs"
let default_outbox = "/var/db/pp-cron/outbox"
let default_store = "/var/db/pp-cron/runs.jsonl"
let default_spool = "/var/db/pp-cron/spool"
let post_timeout = 5.

(* ------------------------------------------------------------------ *)
(* The one HTTP sink: POST, 2xx is an ack, 409 is the receiver's        *)
(* duplicate ack, a read timeout is `Timeout.                            *)
(* ------------------------------------------------------------------ *)

let parse_url url =
  let prefix = "http://" in
  let n = String.length prefix in
  if String.length url <= n || String.sub url 0 n <> prefix then None
  else
    let rest = String.sub url n (String.length url - n) in
    let hostport, path =
      match String.index_opt rest '/' with
      | Some i -> (String.sub rest 0 i, String.sub rest i (String.length rest - i))
      | None -> (rest, "/")
    in
    match String.index_opt hostport ':' with
    | None -> Some (hostport, 80, path)
    | Some i ->
      let host = String.sub hostport 0 i in
      let port = String.sub hostport (i + 1) (String.length hostport - i - 1) in
      (match int_of_string_opt port with Some p -> Some (host, p, path) | None -> None)

let write_all sock s =
  let b = Bytes.unsafe_of_string s in
  let len = Bytes.length b in
  let rec go off = if off < len then go (off + Unix.write sock b off (len - off)) in
  go 0

(* Bounded read: a sink that never answers is a timeout, not a hang. *)
let read_reply sock =
  let buf = Buffer.create 256 in
  let chunk = Bytes.create 4096 in
  let deadline = Unix.gettimeofday () +. post_timeout in
  let rec loop () =
    let left = deadline -. Unix.gettimeofday () in
    if left <= 0. then `Timeout
    else
      match Unix.select [ sock ] [] [] left with
      | exception Unix.Unix_error (e, _, _) -> `Error (Unix.error_message e)
      | [], _, _ -> `Timeout
      | _ ->
        let got = Unix.read sock chunk 0 (Bytes.length chunk) in
        Buffer.add_subbytes buf chunk 0 got;
        if got = 0 then `Body (Buffer.contents buf) else loop ()
  in
  loop ()

let status_of_response s =
  match String.split_on_char ' ' s with
  | _ :: code :: _ -> int_of_string_opt code
  | _ -> None

let http_transport url : Outbox.transport =
  let post ~payload =
    match parse_url url with
    | None -> `Error ("bad sink url: " ^ url)
    | Some (host, port, path) -> (
      match Unix.gethostbyname host with
      | exception Not_found -> `Error ("unknown sink host: " ^ host)
      | h ->
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let close () = try Unix.close sock with Unix.Unix_error _ -> () in
        let finish r =
          close ();
          r
        in
        match Unix.connect sock (Unix.ADDR_INET (h.Unix.h_addr_list.(0), port)) with
        | exception Unix.Unix_error (e, _, _) -> finish (`Error (Unix.error_message e))
        | () -> (
          let req =
            Printf.sprintf
              "POST %s HTTP/1.1\r\nHost: %s\r\nContent-Type: application/json\r\n\
               Content-Length: %d\r\nConnection: close\r\n\r\n%s"
              path host (String.length payload) payload
          in
          match write_all sock req with
          | exception Unix.Unix_error (e, _, _) ->
            finish (`Error (Unix.error_message e))
          | () -> (
            match read_reply sock with
            | `Timeout -> finish `Timeout
            | `Error e -> finish (`Error e)
            | `Body body -> (
              match status_of_response body with
              | Some 409 -> finish (`Ack `Dup)
              | Some c when c >= 200 && c < 300 -> finish (`Ack `Ack)
              | Some c -> finish (`Error (Printf.sprintf "sink status %d" c))
              | None -> finish (`Error "malformed sink response")))))
  in
  { Outbox.post = post }

(* ------------------------------------------------------------------ *)

let () =
  match Array.to_list Sys.argv with
  | _ :: "check" :: rest ->
    let dirs = match rest with [] -> [ "/etc/pp-cron/jobs" ] | l -> l in
    let bad = ref false in
    List.iter
      (fun dir ->
         match Spec.check_dir dir with
         | Ok jobs -> Printf.printf "%s: %d job(s) ok\n" dir (List.length jobs)
         | Error errs ->
           bad := true;
           List.iter (fun e -> prerr_endline ("pp-cron: " ^ e)) errs)
      dirs;
    exit (if !bad then 1 else 0)
  | _ :: "ship" :: rest ->
    let queue =
      match rest with
      | p :: _ -> p
      | [] -> (
        match Sys.getenv_opt "PP_CRON_OUTBOX" with
        | Some p -> p
        | None -> default_outbox)
    in
    let url =
      match Sys.getenv_opt "PP_CRON_SINK" with Some u -> u | None -> default_sink
    in
    let transport = http_transport url in
    (match Outbox.drain_file transport ~path:queue with
     | Ok 0 -> Printf.printf "pp-cron: outbox empty (%s)\n" queue; exit 0
     | Ok n -> Printf.printf "pp-cron: shipped %d record(s)\n" n; exit 0
     | Error e ->
       prerr_endline ("pp-cron: ship: " ^ e);
       exit 1)
  | _ :: "run" :: rest ->
    (* S2: one tick fires every job whose schedule matches the CURRENT
       minute; --watch ticks once a minute. The occurrence is the minute
       served, never the instant the tick happened to run. *)
    let jobs_dir = ref None and store = ref None and watch = ref false in
    let rec flags = function
      | [] -> ()
      | "--jobs" :: d :: tl -> jobs_dir := Some d; flags tl
      | "--store" :: s :: tl -> store := Some s; flags tl
      | "--watch" :: tl -> watch := true; flags tl
      | d :: tl -> jobs_dir := Some d; flags tl
    in
    flags rest;
    let dir =
      match !jobs_dir with
      | Some d -> d
      | None -> (
        match Sys.getenv_opt "PP_CRON_JOBS" with
        | Some d -> d
        | None -> "/etc/pp-cron/jobs")
    in
    let store_path =
      match !store with
      | Some p -> p
      | None -> (
        match Sys.getenv_opt "PP_CRON_STORE" with
        | Some p -> p
        | None -> default_store)
    in
    let spool =
      match Sys.getenv_opt "PP_CRON_SPOOL" with
      | Some p -> p
      | None -> default_spool
    in
    let box = Sched.box_id () in
    let jobs = ref (Spec.load dir) in
    let once () =
      match
        Sched.tick ~store:store_path ~spool ~box ~now:(Unix.gettimeofday ()) !jobs
      with
      | Ok 0 -> 0
      | Ok n ->
        Printf.printf "pp-cron: fired %d job(s)\n" n;
        0
      | Error e ->
        prerr_endline ("pp-cron: run: " ^ e);
        1
    in
    if not !watch then exit (once ())
    else begin
      let rec loop () =
        ignore (once ());
        jobs := Spec.load ~prev:!jobs dir;
        let t = Unix.gettimeofday () in
        ignore (Unix.select [] [] [] (60. -. Float.rem t 60. +. 0.05));
        loop ()
      in
      loop ()
    end
  | _ :: ("status" | "serve") :: _ ->
    prerr_endline
      "pp-cron: not implemented in this card (record + check + ship + run)";
    exit 2
  | _ ->
    prerr_endline usage;
    exit 2
