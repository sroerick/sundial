(* Unit test for the S1 laws in Spec: the run_id exists before exec, a box
   verdict is never `Missing, check accepts a valid dir, a corrupt file is
   refused naming file:line, a short heartbeat and a wide env file are
   refused, and reload is fail-closed (previous set kept). *)

let failures = ref 0

let check name cond =
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

  if !failures = 0 then print_endline "pp-cron spec: all laws hold"
  else begin
    Printf.printf "pp-cron spec: %d failure(s)\n" !failures;
    exit 1
  end
