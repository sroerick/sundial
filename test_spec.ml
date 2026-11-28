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

  if !failures = 0 then Printf.printf "pp-cron spec: %d checks, all laws hold\n" !checks
  else begin
    Printf.printf "pp-cron spec: %d failure(s)\n" !failures;
    exit 1
  end
