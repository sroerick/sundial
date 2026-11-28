(* Unit test for the S1 laws in Spec: the run_id exists before exec, a box
   verdict is never `Missing, check accepts a valid dir, a corrupt file is
   refused naming file:line, a short heartbeat and a wide env file are
   refused, and reload is fail-closed (previous set kept). *)

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if not cond then begin
    incr failures;
    Printf.printf "FAIL %s\n" name
  end

let has_sub hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i =
    i + nl <= hl && (String.sub hay i nl = needle || go (i + 1))
  in
  nl = 0 || go 0
let refused_err name dir needle =
  match Spec.check_dir dir with
  | Ok _ -> check name false
  | Error errs -> check name (List.exists (fun e -> has_sub e needle) errs)

let write_file path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let rec mkdir_p path =
  if path <> "" && path <> "/" && not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let base_dir =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "pp-cron-s1-%d" (Unix.getpid ()))

(* A fresh directory, emptied of schedule files from a previous run. *)
let fresh name =
  let d = Filename.concat base_dir name in
  mkdir_p d;
  (try
     Array.iter
       (fun f ->
          if Filename.check_suffix f ".sexp" || Filename.check_suffix f ".env" then
            try Sys.remove (Filename.concat d f) with Sys_error _ -> ())
       (Sys.readdir d)
   with Sys_error _ -> ());
  d

let good_job =
  {|(job "acme-renew"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/usr/local/bin/acme-client" "-n" "pricklypear.club")
  (timeout 900)
  (overlap skip)
  (on-miss once)
  (retry 2 300)
  (heartbeat 93600)
  (tags ("tls" "club")))
|}

let () =
  (* --- law: run_id is minted before exec; box never yields `Missing --- *)
  let start =
    Spec.begin_run ~box:"gregor" ~job:"acme-renew" ~occurrence:1_700_000_000.
      ~now:1_700_000_001. ()
  in
  check "run_id is a 26-char ULID minted before exec"
    (String.length start.Spec.run_id = 26);
  check "run_id is not empty" (start.Spec.run_id <> "");
  check "box is the stable id, not the hostname" (start.Spec.box = "gregor");
  check "occurrence is the intended fire time"
    (start.Spec.occurrence = 1_700_000_000.);
  check "attempt is 1-based" (start.Spec.attempt = 1);
  let box_side : Spec.box_verdict list = [ `Ok; `Failed "exit 1"; `Timeout ] in
  List.iter
    (fun v ->
       let r =
         Spec.finish_run ~start v ~finished:(start.Spec.started +. 3.)
           ~exit:1 ()
       in
       check "box-side verdict is never Missing" (r.Spec.verdict <> `Missing);
       check "retries keep the run_id" (r.Spec.run_id = start.Spec.run_id))
    box_side;

  (* --- law: check accepts a valid jobs dir --- *)
  let good = fresh "jobs-good" in
  write_file (Filename.concat good "acme-renew.sexp") good_job;
  (match Spec.check_dir good with
   | Ok jobs -> check "check_dir accepts a valid dir" (List.length jobs = 1)
   | Error _ -> check "check_dir accepts a valid dir" false);
  let refused = ref [] in
  let log m = refused := m :: !refused in
  let first = Spec.load ~log good in
  check "valid dir yields its job set" (List.length first = 1);

  (* --- law: a corrupt file is refused naming file:line; reload is closed --- *)
  let bad = fresh "jobs-bad" in
  write_file (Filename.concat bad "acme-renew.sexp") good_job;
  write_file (Filename.concat bad "corrupt.sexp")
    {|(job "typo"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (overlap skpi)
  (heartbeat 93600))
|};
  (match Spec.check_dir bad with
   | Ok _ -> check "check_dir refuses a corrupt file" false
   | Error errs ->
     check "corrupt file refusal names file:line"
       (List.exists (fun e -> has_sub e "corrupt.sexp:5:") errs));
  refused := [];
  let second = Spec.load ~prev:first ~log bad in
  check "fail closed: previous job set is kept" (second = first);
  check "refusal logged with file:line"
    (List.exists (fun m -> has_sub m "corrupt.sexp:5:") !refused);

  (* --- law: heartbeat must cover the schedule cadence --- *)
  let hb = fresh "jobs-heartbeat" in
  write_file (Filename.concat hb "tight.sexp")
    {|(job "tight"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (heartbeat 3600))
|};
  (match Spec.check_dir hb with
   | Ok _ -> check "heartbeat smaller than cadence refused" false
   | Error errs ->
     check "heartbeat refusal is named"
       (List.exists (fun e -> has_sub (String.lowercase_ascii e) "heartbeat") errs));

  (* --- law: env-from must be mode 0600 and daemon-owned --- *)
  let envd = fresh "jobs-env" in
  let envf = Filename.concat envd "withevn.env" in
  write_file envf "TOKEN=abc\n";
  Unix.chmod envf 0o644;
  write_file (Filename.concat envd "withevn.sexp")
    (Printf.sprintf
       {|(job "withevn"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (timeout 900)
  (env-from "%s")
  (heartbeat 93600))
|}
       envf);
  (match Spec.check_dir envd with
   | Ok _ -> check "env-from mode 0644 is refused" false
   | Error errs ->
     check "env-from refusal names the mode" (List.exists (fun e -> has_sub e "0600") errs));
  Unix.chmod envf 0o600;
  (match Spec.check_dir envd with
   | Ok jobs -> check "env-from mode 0600 is accepted" (List.length jobs = 1)
   | Error _ -> check "env-from mode 0600 is accepted" false);

  (* --- law: every record carries box-emitted evidence; ingest checks it --- *)
  let store = fresh "store" in
  let record =
    Spec.finish_run ~start `Ok ~finished:(start.Spec.started +. 3.) ~exit:0
      ~log_tail:"renewed\n" ()
  in
  check "the box emits evidence with the record" (record.Spec.evidence <> "");
  (match Spec.ingest store record with
   | Ok () -> check "ingest accepts a record whose evidence matches" true
   | Error e -> check ("ingest accepts a record whose evidence matches: " ^ e) false);
  check "verify over untouched stored records yields no complaints"
    (Spec.verify_store store = []);

  (* --- law: an altered stored record is named by run_id, exit non-zero --- *)
  let tampered = { record with Spec.verdict = `Failed "rewritten" } in
  Spec.write_record store tampered;
  (match Spec.verify_store store with
   | [] -> check "verify names an altered record by run_id" false
   | errs ->
     check "verify names an altered record by run_id"
       (List.exists (fun e -> has_sub e record.Spec.run_id) errs));

  (* --- law: ingest rejects a record whose evidence fails, stores nothing --- *)
  let store2 = fresh "store-reject" in
  refused := [];
  (match Spec.ingest ~log store2 tampered with
   | Ok () -> check "ingest rejects a record whose evidence fails" false
   | Error _ -> check "ingest rejects a record whose evidence fails" true);
  check "rejection is logged" (List.exists (fun m -> has_sub m "REJECTED") !refused);
  check "a rejected record is not stored"
    (not (Sys.file_exists (Filename.concat store2 (record.Spec.run_id ^ ".run"))));
  check "an empty store verifies clean" (Spec.verify_store store2 = []);

  (* --- law (run-record): one record definition, field laws, wire round-trip --- *)
  let rec_dir = fresh "jobs-record" in
  write_file (Filename.concat rec_dir "rich.sexp")
    {|(job "rich"
  (schedule "*/5 0-6 * * 1-5")
  (tz "America/Denver")
  (cmd "/bin/echo" "hi")
  (timeout 60)
  (overlap kill 5)
  (on-miss all)
  (retry 3 30)
  (heartbeat 3600)
  (tags ("a" "b")))
|};
  (match Spec.check_dir rec_dir with
   | Ok [ j ] ->
     check "record: schedule is the cron expression"
       (j.Spec.schedule = "*/5 0-6 * * 1-5");
     check "record: tz is explicit" (j.Spec.tz = "America/Denver");
     check "record: cmd is argv, not a shell line" (j.Spec.cmd = [ "/bin/echo"; "hi" ]);
     check "record: timeout in seconds" (j.Spec.timeout = 60.);
     check "record: overlap kill carries its grace" (j.Spec.overlap = `Kill 5.);
     check "record: on-miss is a policy, not a bool" (j.Spec.on_miss = `All);
     check "record: retry is attempts and delay" (j.Spec.retry = Some (3, 30.));
     check "record: tags" (j.Spec.tags = [ "a"; "b" ]);
     check "record: heartbeat declared" (j.Spec.heartbeat = 3600.)
   | _ -> check "record: one file defines one job" false);
  let occ = 1_700_000_000. in
  let late = Spec.begin_run ~box:"gregor" ~job:"rich" ~occurrence:occ ~now:(occ +. 40.) () in
  check "occurrence is the intended fire, not the actual start"
    (late.Spec.occurrence = occ && late.Spec.started = occ +. 40.);
  check "box is the stable id handed in, never a hostname" (late.Spec.box = "gregor");
  let retry_start =
    Spec.begin_run ~box:"gregor" ~job:"rich" ~occurrence:occ ~now:(occ +. 41.)
      ~run_id:late.Spec.run_id ~attempt:2 ()
  in
  check "a retry keeps one run_id and changes only attempt"
    (retry_start.Spec.run_id = late.Spec.run_id && retry_start.Spec.attempt = 2);
  check "run_id is time-sortable"
    (String.compare (Spec.mint_run_id ~now:1_600_000_000.)
       (Spec.mint_run_id ~now:1_700_000_000.)
    < 0);
  let long = String.make 10_000 'x' ^ "TAIL" in
  let rec_run =
    Spec.finish_run ~start:late `Ok ~finished:(occ +. 43.) ~exit:0 ~log_tail:long ()
  in
  check "log_tail is bounded to 8 KiB" (String.length rec_run.Spec.log_tail = 8192);
  check "log_tail keeps the end of the output" (has_sub rec_run.Spec.log_tail "TAIL");
  let rt = fresh "record-roundtrip" in
  Spec.write_record rt rec_run;
  (match Spec.read_record (Filename.concat rt (rec_run.Spec.run_id ^ ".run")) with
   | Error e -> check ("record round-trips through the wire form: " ^ e) false
   | Ok back ->
     check "record round-trips through the wire form" ((back : Spec.run) = rec_run));
  check "evidence covers the verdict"
    (not (Spec.record_matches_evidence { rec_run with Spec.verdict = `Failed "rewritten" }));
  check "evidence covers the attempt"
    (not (Spec.record_matches_evidence { rec_run with Spec.attempt = 9 }));

  (* --- law (schedule-files): one job per file, unknown keys and values refused --- *)
  let two = fresh "jobs-two-forms" in
  write_file (Filename.concat two "two.sexp") (good_job ^ good_job);
  refused_err "two (job ...) forms in one file are refused" two
    "expected exactly one (job ...) form";
  let unkkey = fresh "jobs-unknown-key" in
  write_file (Filename.concat unkkey "unk.sexp")
    {|(job "unk"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (timeout 60)
  (heartbeat 93600)
  (retries 3))
|};
  refused_err "an unknown key is refused, never ignored" unkkey "unknown key";
  let dupkey = fresh "jobs-duplicate-key" in
  write_file (Filename.concat dupkey "dup.sexp")
    {|(job "dup"
  (schedule "17 3 * * *")
  (schedule "18 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (timeout 60)
  (heartbeat 93600))
|};
  refused_err "a duplicate key is refused" dupkey "duplicate key";
  let dupname = fresh "jobs-duplicate-name" in
  write_file (Filename.concat dupname "a.sexp") good_job;
  write_file (Filename.concat dupname "b.sexp") good_job;
  refused_err "a duplicate job name across files is refused" dupname "duplicate job name";
  let missing = fresh "jobs-missing-key" in
  write_file (Filename.concat missing "missing.sexp")
    {|(job "missing"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (heartbeat 93600))
|};
  refused_err "a missing required key is refused" missing "missing required key";

  (* --- law (declared-jobs): the two real jobs validate as declared --- *)
  (match Spec.check_dir "jobs" with
   | Ok jobs ->
     let names = List.sort compare (List.map (fun (j : Spec.job) -> j.Spec.name) jobs) in
     check "sundial/jobs declares factory-monitor and backup-agent-memory"
       (names = [ "backup-agent-memory"; "factory-monitor" ]);
     (match List.find_opt (fun (j : Spec.job) -> j.Spec.name = "factory-monitor") jobs with
      | None -> check "factory-monitor is declared" false
      | Some j ->
        check "factory-monitor fires every fifteen minutes"
          (j.Spec.schedule = "*/15 * * * *");
        check "factory-monitor heartbeat covers its cadence" (j.Spec.heartbeat = 3600.));
     (match List.find_opt (fun (j : Spec.job) -> j.Spec.name = "backup-agent-memory") jobs with
      | None -> check "backup-agent-memory is declared" false
      | Some j ->
        check "backup-agent-memory is weekly, Monday 09:00"
          (j.Spec.schedule = "0 9 * * 1");
        check "backup-agent-memory carries an explicit tz, no host"
          (j.Spec.tz = "America/Denver");
        check "backup-agent-memory heartbeat covers a week" (j.Spec.heartbeat = 691200.))
   | Error errs -> check ("sundial/jobs validates: " ^ String.concat "; " errs) false);

  (* --- law (non-goals): refused surfaces stay refused --- *)
  let ng = fresh "jobs-nongoal" in
  write_file (Filename.concat ng "auto.sexp")
    {|(job "auto"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (timeout 60)
  (heartbeat 93600)
  (auto-disable true))
|};
  refused_err "auto-disable is refused as a policy" ng "unknown key";
  let dag = fresh "jobs-dag" in
  write_file (Filename.concat dag "dag.sexp")
    {|(job "dag"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (timeout 60)
  (heartbeat 93600)
  (depends-on "other"))
|};
  refused_err "cross-job dependencies are refused" dag "unknown key";
  let quiet c = Sys.command (c ^ " >/dev/null 2>&1") in
  check "there is no per-box web server" (quiet "./pp_cron.exe serve" = 2);
  check "there is no command or dispatch surface" (quiet "./pp_cron.exe run" = 2);

  (* --- law (policies): overlap, catch-up, retry and ok-codes --- *)
  check "overlap: a free slot dispatches" (Policies.overlap_action `Skip `Done = `Dispatch);
  check "overlap: skip records the firing instead of running it"
    (Policies.overlap_action `Skip (`Running 5.) = `Skip_recorded);
  check "overlap: queue serializes one deep"
    (Policies.overlap_action `Queue (`Running 5.) = `Queue);
  check "overlap: kill names its grace"
    (Policies.overlap_action (`Kill 30.) (`Running 5.) = `Kill_after 30.);
  check "a skipped firing is recorded, not dropped"
    (Policies.skipped_verdict = `Failed "overlap-skip");
  check "a policy-killed firing is recorded as such"
    (Policies.killed_verdict = `Failed "overlap-kill");
  let missed = [ 1.; 2.; 3.; 4. ] in
  check "on-miss skip runs nothing late" (Policies.catch_up `Skip ~missed = []);
  check "on-miss once runs the latest missed occurrence"
    (Policies.catch_up `Once ~missed = [ 4. ]);
  check "on-miss all replays every missed occurrence"
    (Policies.catch_up `All ~missed = missed);
  check "retry: attempt 1 of 2 has one retry left"
    (Policies.retry_action ~retry:(Some (2, 300.)) ~attempt:1 = `Retry_after 300.);
  check "retry: the last allowed attempt is final"
    (Policies.retry_action ~retry:(Some (2, 300.)) ~attempt:2 = `Final);
  check "retry: no retry declared means final"
    (Policies.retry_action ~retry:None ~attempt:1 = `Final);
  check "retry only covers failure, never success"
    (Policies.after_verdict ~retry:(Some (2, 300.)) ~attempt:1 `Ok = `Final);
  check "retry covers a timeout"
    (Policies.after_verdict ~retry:(Some (2, 300.)) ~attempt:1 `Timeout
    = `Retry_after 300.);
  check "exit 0 is success" (Policies.verdict_of_exit ~ok_codes:[] 0 = `Ok);
  check "a non-zero exit is a failure"
    (Policies.verdict_of_exit ~ok_codes:[] 3 = `Failed "exit 3");
  check "a declared ok-code is success"
    (Policies.verdict_of_exit ~ok_codes:[ 2; 3 ] 3 = `Ok);
  let okc = fresh "jobs-okcodes" in
  write_file (Filename.concat okc "ok.sexp")
    {|(job "okc"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (timeout 60)
  (ok-codes (0 2))
  (heartbeat 93600))
|};
  (match Spec.check_dir okc with
   | Ok [ j ] -> check "record: ok-codes is a declared list" (j.Spec.ok_codes = [ 0; 2 ])
   | _ -> check "record: ok-codes is a declared list" false);

  (* --- law (env-and-secrets): KEY=VALUE, no expansion, minimal inheritance --- *)
  (match Envfile.parse ~file:"t.env" "A=1\nB=two=2\n# c\n\nC=$HOME\n" with
   | Ok [ ("A", "1"); ("B", "two=2"); ("C", "$HOME") ] ->
     check "env: KEY=VALUE, no expansion, comments skipped" true
   | Ok _ -> check "env: KEY=VALUE, no expansion, comments skipped" false
   | Error e -> check ("env: " ^ e) false);
  (match Envfile.parse ~file:"bad.env" "NOEQUALS\n" with
   | Error e ->
     check "env: a line without '=' is refused naming file:line"
       (has_sub e "bad.env:1:")
   | Ok _ -> check "env: a line without '=' is refused naming file:line" false);
  (match Envfile.parse ~file:"bad.env" "1A=1\n" with
   | Error _ -> check "env: a non-KEY left side is refused" true
   | Ok _ -> check "env: a non-KEY left side is refused" false);
  let envdir = fresh "jobs-envtest" in
  write_file (Filename.concat envdir "envtest.sexp")
    {|(job "envtest"
  (schedule "17 3 * * *")
  (tz "America/Denver")
  (cmd "/bin/true")
  (timeout 60)
  (heartbeat 93600))
|};
  (match Spec.check_dir envdir with
   | Ok [ j ] ->
     let s =
       Spec.begin_run ~box:"gregor" ~job:"envtest" ~occurrence:1_700_000_000.
         ~now:1_700_000_001. ()
     in
     let base =
       [ ("FOO", "bar"); ("TZ", "evil"); ("RUN_ID", "evil"); ("PATH", "/opt/bin") ]
     in
     let env =
       Envfile.child_env
         ~daemon:[| "PATH=/usr/bin:/bin"; "HOME=/home/_ppcron"; "TERM=xterm" |]
         ~base j s
     in
     let get k = List.assoc_opt k env in
     check "env: TZ is the job's declared tz" (get "TZ" = Some "America/Denver");
     check "env: RUN_ID is the minted id, not the file's"
       (get "RUN_ID" = Some s.Spec.run_id);
     check "env: PP_CRON_OCCURRENCE is the intended fire"
       (get "PP_CRON_OCCURRENCE" = Some "1700000000");
     check "env: PP_CRON_ATTEMPT is 1-based" (get "PP_CRON_ATTEMPT" = Some "1");
     check "env: PP_CRON_BOX is the stable box id" (get "PP_CRON_BOX" = Some "gregor");
     check "env: the job file's variables are passed" (get "FOO" = Some "bar");
     check "env: a file's PATH overrides the inherited one"
       (get "PATH" = Some "/opt/bin");
     check "env: no TERM leaks from the daemon" (get "TERM" = None);
     check "env: no duplicate keys"
       (List.length env
       = List.length (List.sort_uniq compare (List.map fst env)))
   | _ -> check "env: a job parses for the env test" false);

  (* --- law (triage): one openness per (box, job, failure_class) window --- *)
  let failed_run ?(job = "poison") ?(started = 1_700_000_000.) reason =
    let s = Spec.begin_run ~box:"gregor" ~job ~occurrence:started ~now:started () in
    Spec.finish_run ~start:s (`Failed reason) ~finished:(started +. 1.) ~exit:1 ()
  in
  let open_ = ref [] and opened = ref 0 in
  List.iter
    (fun i ->
      let r = failed_run ~started:(1_700_000_000. +. (float_of_int i *. 60.)) "exit 1" in
      match Triage.decide ~window:86400. ~open_:!open_ r with
      | `Open cls ->
        incr opened;
        open_ :=
          { Triage.box = r.Spec.box;
            job = r.Spec.job;
            failure_class = cls;
            opened_at = r.Spec.started }
          :: !open_
      | `Annotate _ | `Ignore -> ())
    (List.init 399 (fun i -> i));
  check "triage: 399 failures of one class open one card" (!opened = 1);
  let inside = failed_run ~started:(1_700_000_000. +. (399. *. 60.)) "exit 1" in
  check "triage: a failure inside the window annotates the open card"
    (match Triage.decide ~window:86400. ~open_:!open_ inside with
     | `Annotate _ -> true
     | _ -> false);
  let after_window = failed_run ~started:(1_700_000_000. +. 90_000.) "exit 1" in
  check "triage: a failure after the window opens a new card"
    (match Triage.decide ~window:86400. ~open_:!open_ after_window with
     | `Open _ -> true
     | _ -> false);
  let other_class = failed_run ~started:(1_700_000_100.) "timeout" in
  check "triage: a different failure class opens its own card"
    (match Triage.decide ~window:86400. ~open_:!open_ other_class with
     | `Open _ -> true
     | _ -> false);
  let ok_run =
    Spec.finish_run
      ~start:(Spec.begin_run ~box:"gregor" ~job:"poison" ~occurrence:1. ~now:1. ())
      `Ok ~finished:2. ()
  in
  check "triage: an ok run opens nothing"
    (Triage.decide ~open_:[] ok_run = `Ignore);

  (* --- law (outbox): batch, backoff, dead-letter, backpressure --- *)
  let mkrow i =
    { Outbox.run_id = Printf.sprintf "r%03d" i;
      payload = "{}";
      created_at = float_of_int i;
      attempts = 0 }
  in
  let rows = List.init 250 mkrow in
  let b = Outbox.batch rows in
  check "outbox: a batch is capped at 200 rows" (List.length b = 200);
  check "outbox: a batch takes the oldest rows first"
    (List.map (fun r -> int_of_float r.Outbox.created_at) b
    = List.init 200 (fun i -> i));
  check "outbox: the rest stay pending" (List.length rows - List.length b = 50);
  check "outbox: backoff doubles from one second"
    (List.map Outbox.base_delay [ 1; 2; 3; 4 ] = [ 1.; 2.; 4.; 8. ]);
  check "outbox: backoff stops at the five-minute ceiling"
    (Outbox.base_delay 20 = 300.);
  check "outbox: jitter never passes the ceiling"
    (Outbox.backoff ~rand:(fun () -> 0.999) ~attempts:20 <= 300.
    && Outbox.backoff ~rand:(fun () -> 0.) ~attempts:1 = 0.5);
  let row = mkrow 7 in
  check "outbox: a 2xx deletes the row"
    (Outbox.decide ~rand:(fun () -> 0.5) row `Ok = `Delete);
  check "outbox: a 5xx retries with backoff"
    (match Outbox.decide ~rand:(fun () -> 0.5) row (`Server_error 503) with
     | `Retry_after d -> d > 0. && d <= 300.
     | _ -> false);
  check "outbox: a transport error retries"
    (match Outbox.decide ~rand:(fun () -> 0.5) row (`Transport "eof") with
     | `Retry_after _ -> true
     | _ -> false);
  check "outbox: a 4xx is retried, not dropped"
    (match Outbox.decide ~rand:(fun () -> 0.5) row (`Client_error 400) with
     | `Retry_after _ -> true
     | _ -> false);
  let twice = { row with Outbox.attempts = 2 } in
  check "outbox: a 4xx that repeats three times is dead-lettered"
    (match Outbox.decide ~rand:(fun () -> 0.5) twice (`Client_error 400) with
     | `Dead_letter msg -> has_sub msg "400"
     | _ -> false);
  check "outbox: pressure is announced above the threshold"
    (Outbox.backpressured ~pending:10_001 ());
  check "outbox: pressure is silent at the threshold"
    (not (Outbox.backpressured ~pending:10_000 ()));

  (* --- law (scheduler): heap order, occurrences in tz, replay, flock --- *)
  let h = Sched.create () in
  Sched.push h (30., "c");
  Sched.push h (10., "a");
  Sched.push h (20., "b");
  check "scheduler: the heap peeks the earliest deadline" (Sched.peek h = Some (10., "a"));
  check "scheduler: the heap pops in deadline order"
    (Sched.pop h = Some (10., "a")
    && Sched.pop h = Some (20., "b")
    && Sched.pop h = Some (30., "c")
    && Sched.pop h = None);
  let daily = Spec.parse_cron ~file:"test" ~line:0 "17 3 * * *" in
  let jan15 = 1_768_435_200. in
  check "scheduler: an occurrence is computed on the declared clock"
    (Sched.next_occurrence ~tz:"UTC" ~cron:daily ~after:jan15 () = Some 1_768_447_020.);
  check "scheduler: the next occurrence is strictly after the last one"
    (Sched.next_occurrence ~tz:"UTC" ~cron:daily ~after:1_768_447_020. ()
    = Some 1_768_533_420.);
  check "scheduler: a tz shifts the occurrence onto its wall clock"
    (Sched.next_occurrence ~tz:"America/Denver" ~cron:daily ~after:jan15 ()
    = Some 1_768_472_220.);
  check "scheduler: the tz occurrence reads back as 03:17 local"
    (let tm = Sched.local_tm "America/Denver" 1_768_472_220. in
     tm.Unix.tm_hour = 3 && tm.Unix.tm_min = 17);
  let dst_gap = Spec.parse_cron ~file:"test" ~line:0 "30 2 * * *" in
  let mar8_midnight_mst = 1_772_953_200. in
  check "scheduler: a local time skipped by DST does not fire that day"
    (Sched.next_occurrence ~tz:"America/Denver" ~cron:dst_gap
         ~after:mar8_midnight_mst ()
    = Some 1_773_045_000.);
  check "scheduler: the DST-gap job fires at 02:30 the next day"
    (let tm = Sched.local_tm "America/Denver" 1_773_045_000. in
     tm.Unix.tm_mday = 9 && tm.Unix.tm_hour = 2 && tm.Unix.tm_min = 30);
  let quarter = Spec.parse_cron ~file:"test" ~line:0 "*/15 * * * *" in
  let two_hours = Sched.replay ~tz:"UTC" ~cron:quarter ~last_fired:jan15
      ~now:(jan15 +. 7200.) () in
  check "scheduler: replay from the last fire gives every due occurrence"
    (List.length two_hours = 8);
  check "scheduler: replay occurrences match the schedule"
    (List.hd two_hours = jan15 +. 900.
    && List.nth two_hours 7 = jan15 +. 7200.);
  check "scheduler: a clock step back does not storm"
    (Sched.replay ~tz:"UTC" ~cron:quarter ~last_fired:jan15
       ~now:(jan15 +. 600.) ()
    = []);
  check "scheduler: replay after the step resumes with no gap"
    (Sched.replay ~tz:"UTC" ~cron:quarter ~last_fired:(jan15 +. 7200.)
       ~now:(jan15 +. 14_400.) ()
    = List.init 8 (fun i -> jan15 +. (900. *. float_of_int (i + 9))));
  let lockdir = fresh "lock" in
  let pidfile = Filename.concat lockdir "ppcron.pid" in
  (match Sched.single_instance pidfile with
   | `Acquired lock ->
     check "scheduler: the first daemon takes the pidfile lock" true;
     let child = Unix.fork () in
      if child = 0 then begin
        match Sched.single_instance pidfile with
        | `Running -> Unix._exit 0
        | `Acquired _ -> Unix._exit 3
      end
      else begin
        let _, status = Unix.waitpid [] child in
        check "scheduler: a second start exits instead of fighting for the heap"
          (status = Unix.WEXITED 0)
      end;
     Sched.release lock;
     check "scheduler: the lock is free after release"
       (match Sched.single_instance pidfile with
        | `Acquired l -> Sched.release l; true
        | `Running -> false)
   | `Running -> check "scheduler: the first daemon takes the pidfile lock" false);

  (* --- law (executor): process group, deadline, spool, env contract --- *)
  let spool = fresh "spool" in
  let ok = Exec.run ~spool_dir:spool ~timeout:5. ~cmd:[ "/bin/sh"; "-c"; "echo hello" ] () in
  check "executor: a clean exit is ok" (ok.Exec.verdict = `Ok && ok.Exec.exit = Some 0);
  check "executor: the spool tail carries the output" (has_sub ok.Exec.log_tail "hello");
  check "executor: the byte count is the true size" (ok.Exec.log_bytes = 6);
  let failed = Exec.run ~spool_dir:spool ~timeout:5. ~cmd:[ "/bin/sh"; "-c"; "exit 4" ] () in
  check "executor: a non-zero exit is a failure"
    (failed.Exec.verdict = `Failed "exit 4");
  let allowed =
    Exec.run ~ok_codes:[ 4 ] ~spool_dir:spool ~timeout:5.
      ~cmd:[ "/bin/sh"; "-c"; "exit 4" ] ()
  in
  check "executor: a declared ok-code is success" (allowed.Exec.verdict = `Ok);
  let marker = Filename.concat spool "grandchild-alive" in
  let script = Printf.sprintf "(sleep 1; echo alive > %s) & sleep 30" marker in
  let t =
    Exec.run ~grace:0.3 ~spool_dir:spool ~timeout:0.3
      ~cmd:[ "/bin/sh"; "-c"; script ] ()
  in
  check "executor: the deadline verdict is timeout" (t.Exec.verdict = `Timeout);
  check "executor: the timeout is recorded" t.Exec.timed_out;
  Unix.sleepf 1.2;
  check "executor: killing the process group ends the grandchild"
    (not (Sys.file_exists marker));
  let chatty =
    Exec.run ~spool_dir:spool ~timeout:10.
      ~cmd:[ "/bin/sh"; "-c"; "dd if=/dev/zero bs=1024 count=1024 2>/dev/null" ] ()
  in
  check "executor: a chatty job does not deadlock" (chatty.Exec.verdict = `Ok);
  check "executor: log_bytes reports the true size" (chatty.Exec.log_bytes = 1_048_576);
  check "executor: log_tail is the last 8 KiB"
    (String.length chatty.Exec.log_tail = 8192);
  let envjob =
    let dir = fresh "jobs-execenv" in
    write_file (Filename.concat dir "execenv.sexp")
      {|(job "execenv"
  (schedule "17 3 * * *")
  (tz "UTC")
  (cmd "/bin/true")
  (timeout 60)
  (heartbeat 93600))
|};
    match Spec.check_dir dir with
    | Ok [ j ] -> j
    | _ -> failwith "exec env job did not parse"
  in
  let es =
    Spec.begin_run ~box:"gregor" ~job:"execenv" ~occurrence:1_700_000_000.
      ~now:1_700_000_001. ()
  in
  let child_env =
    Envfile.child_env
      ~daemon:[| "PATH=/usr/bin:/bin"; "HOME=/nonexistent" |]
      ~base:[ ("FOO", "bar") ] envjob es
  in
  let e =
    Exec.run ~env:(Some child_env) ~spool_dir:spool ~timeout:5.
      ~cmd:[ "/bin/sh"; "-c"; "printf '%s %s' \"$RUN_ID\" \"$FOO\"" ] ()
  in
  check "executor: the RUN_ID contract and env file reach the child"
    (e.Exec.log_tail = es.Spec.run_id ^ " bar");

  (* --- law (heartbeat): liveness and absence synthesis stay apart --- *)
  let daily9 = Spec.parse_cron ~file:"test" ~line:0 "0 9 * * *" in
  let exp =
    [ { Heartbeat.job = "daily";
        cron = daily9;
        tz = "UTC";
        heartbeat = 93_600. } ]
  in
  let beats from until =
    let rec go t acc = if t > until then List.rev acc else go (t +. 60.) (t :: acc) in
    go from []
  in
  let jan15_09 = 1_768_467_600. and jan16_09 = 1_768_554_000. in
  let seen = [ ("daily", jan15_09) ] in
  check "heartbeat: a late fire does not synthesize a missing row"
    (Heartbeat.missing ~since:jan15 ~now:(jan15_09 +. (25. *. 3600.))
       ~expectations:exp ~observed:seen
       ~beats:(beats jan15 (jan15_09 +. (25. *. 3600.)))
    = []);
  check "heartbeat: the row appears only after the declared gap"
    (Heartbeat.missing ~since:jan15 ~now:(jan15_09 +. (51. *. 3600.))
       ~expectations:exp ~observed:seen
       ~beats:(beats jan15 (jan15_09 +. (51. *. 3600.)))
    = [ ("daily", jan16_09) ]);
  check "heartbeat: a dead box yields no missing-run rows"
    (Heartbeat.missing ~since:jan15 ~now:(jan15_09 +. (51. *. 3600.))
       ~expectations:exp ~observed:seen ~beats:(beats jan15 jan15_09)
    = []);
  check "heartbeat: liveness is 3x the interval"
    (Heartbeat.liveness ~last_beat:(Some 1_000.) ~now:1_179. = `Alive
    && Heartbeat.liveness ~last_beat:(Some 1_000.) ~now:1_181. = `Gap);
  check "heartbeat: no beat at all is a gap"
    (Heartbeat.liveness ~last_beat:None ~now:1_000. = `Gap);
  let beat =
    { Heartbeat.box = "gregor";
      ts = jan15_09;
      uptime_s = 3600.;
      jobs_loaded = 2;
      pending_outbox = 5;
      in_flight = 1 }
  in
  check "heartbeat: the payload names the box and its counts"
    (has_sub (Heartbeat.payload beat) "\"box\":\"gregor\""
    && has_sub (Heartbeat.payload beat) "\"pending_outbox\":5");

  if !failures = 0 then Printf.printf "pp-cron spec: %d checks, all laws hold\n" !checks
  else begin
    Printf.printf "pp-cron spec: %d failure(s)\n" !failures;
    exit 1
  end
