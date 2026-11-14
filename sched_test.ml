(* The S2 gates (card 9f2fd78a): a schedule matching the current minute
   appends exactly one record with a ULID run_id and the intended
   occurrence; a schedule that does not match appends nothing and leaves
   the store untouched; two jobs in the same minute append two records with
   distinct run_ids; a failing handler records a failed verdict and the
   next tick still appends; retries keep one run_id across attempts. *)

let failures = ref 0

let check name cond = if not cond then (incr failures; Printf.printf "FAIL %s\n" name)

let rec mkdir_p path =
  if path <> "" && path <> "/" && not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let base =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "pp-cron-s2-%d" (Unix.getpid ()))

let fresh name =
  let d = Filename.concat base name in
  mkdir_p d;
  (try
     Array.iter
       (fun f ->
          if Filename.check_suffix f ".sexp" || Filename.check_suffix f ".store"
          then try Sys.remove (Filename.concat d f) with Sys_error _ -> ())
       (Sys.readdir d)
   with Sys_error _ -> ());
  d

let job_file ~name ~schedule ?(cmd = "exit 0") ?(retry = "") () =
  Printf.sprintf
    "(job \"%s\"\n  (schedule \"%s\")\n  (tz \"UTC\")\n  \
     (cmd \"/bin/sh\" \"-c\" \"%s\")\n  (timeout 30)\n%s  (heartbeat 90000)\n  \
     (tags ()))\n"
    name schedule cmd retry

let jobs_of dir =
  match Spec.check_dir dir with
  | Ok jobs -> jobs
  | Error errs ->
    List.iter (fun e -> prerr_endline ("pp-cron: " ^ e)) errs;
    incr failures;
    []

let rows path =
  match Store.open_store ~path with
  | Error e ->
    prerr_endline ("pp-cron: store: " ^ e);
    incr failures;
    []
  | Ok t -> (
    match Store.read_all t with
    | Ok l -> l
    | Error e ->
      prerr_endline ("pp-cron: store: " ^ e);
      incr failures;
      [])

let tick ~store ~spool ~now jobs =
  match Sched.tick ~store ~spool ~box:"test-box" ~now jobs with
  | Ok n -> n
  | Error e ->
    prerr_endline ("pp-cron: run: " ^ e);
    incr failures;
    -1

let () =
  let spool = Filename.concat base "spool" in
  let t0 = Unix.gettimeofday () in
  let tm = Unix.gmtime t0 in
  let now_schedule =
    Printf.sprintf "%d %d * * *" tm.Unix.tm_min tm.Unix.tm_hour
  in
  (* A schedule that cannot match this minute: same hour, minute +30. *)
  let other_schedule =
    Printf.sprintf "%d %d * * *" ((tm.Unix.tm_min + 30) mod 60) tm.Unix.tm_hour
  in

  (* R1: matching schedule -> exactly one record. *)
  let d1 = fresh "r1" in
  write_file (Filename.concat d1 "one.sexp")
    (job_file ~name:"one" ~schedule:now_schedule ());
  let store1 = Filename.concat d1 "runs.store" in
  let n1 = tick ~store:store1 ~spool ~now:t0 (jobs_of d1) in
  let rs1 = rows store1 in
  check "R1: one firing" (n1 = 1);
  check "R1: exactly one record" (List.length rs1 = 1);
  List.iter
    (fun (r : Spec.run) ->
       check "R1: run_id is a 26-char ULID" (String.length r.run_id = 26);
       check "R1: occurrence is the intended minute"
         (r.occurrence = Sched.minute_floor t0);
       check "R1: verdict ok" (r.verdict = `Ok);
       check "R1: attempt 1" (r.attempt = 1);
       check "R1: box is the stable id" (r.box = "test-box");
       check "R1: job recorded" (r.job = "one"))
    rs1;

  (* R2: no match -> nothing appended, exit 0, no error. *)
  let d2 = fresh "r2" in
  write_file (Filename.concat d2 "off.sexp")
    (job_file ~name:"off" ~schedule:other_schedule ());
  let store2 = Filename.concat d2 "runs.store" in
  let n2 = tick ~store:store2 ~spool ~now:t0 (jobs_of d2) in
  check "R2: nothing fired" (n2 = 0);
  check "R2: store untouched" (not (Sys.file_exists store2));

  (* R3: two jobs in the same minute -> two records, distinct run_ids. *)
  let d3 = fresh "r3" in
  write_file (Filename.concat d3 "a.sexp")
    (job_file ~name:"a" ~schedule:now_schedule ());
  write_file (Filename.concat d3 "b.sexp")
    (job_file ~name:"b" ~schedule:now_schedule ());
  let store3 = Filename.concat d3 "runs.store" in
  let n3 = tick ~store:store3 ~spool ~now:t0 (jobs_of d3) in
  let rs3 = rows store3 in
  check "R3: two firings" (n3 = 2);
  check "R3: two records" (List.length rs3 = 2);
  (match rs3 with
   | [ x; y ] -> check "R3: distinct run_ids" (x.Spec.run_id <> y.Spec.run_id)
   | _ -> check "R3: two records to compare" false);

  (* R4: handler fails -> failed verdict; the next tick still appends. *)
  let d4 = fresh "r4" in
  write_file (Filename.concat d4 "bad.sexp")
    (job_file ~name:"bad" ~schedule:now_schedule ~cmd:"exit 7" ());
  let store4 = Filename.concat d4 "runs.store" in
  let _n4a = tick ~store:store4 ~spool ~now:t0 (jobs_of d4) in
  let rs4a = rows store4 in
  (match rs4a with
   | [ r ] -> check "R4: failed verdict named" (r.Spec.verdict = `Failed "exit 7")
   | _ -> check "R4: one failed record" false);
  let _n4b = tick ~store:store4 ~spool ~now:t0 (jobs_of d4) in
  let rs4b = rows store4 in
  check "R4: next matching tick appends" (List.length rs4b = 2);

  (* Retries: every attempt appends, one run_id across them. *)
  let d5 = fresh "r5" in
  write_file (Filename.concat d5 "flaky.sexp")
    (job_file ~name:"flaky" ~schedule:now_schedule ~cmd:"exit 2"
       ~retry:"  (retry 3 0)\n" ());
  let store5 = Filename.concat d5 "runs.store" in
  let _n5 = tick ~store:store5 ~spool ~now:t0 (jobs_of d5) in
  let rs5 = rows store5 in
  check "retry: one record per attempt" (List.length rs5 = 3);
  (match rs5 with
   | a :: b :: c :: _ ->
     check "retry: same run_id across attempts"
       (a.Spec.run_id = b.Spec.run_id && b.Spec.run_id = c.Spec.run_id);
     check "retry: attempts 1,2,3"
       (a.Spec.attempt = 1 && b.Spec.attempt = 2 && c.Spec.attempt = 3)
   | _ -> check "retry: three attempts to compare" false);

  if !failures = 0 then
    Printf.printf "pp-cron s2: scheduler + executor: all laws hold\n"
  else begin
    Printf.printf "pp-cron s2: %d failure(s)\n" !failures;
    exit 1
  end
