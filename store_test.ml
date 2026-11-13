(* Unit test for the S3a laws in Store: N tick records inserted round-trip
   byte-equal (run_id, occurrence, verdict and every other field), and a
   corrupt or torn store file is refused with an explicit error, never a
   crash. *)

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
    (Printf.sprintf "pp-cron-s3a-%d" (Unix.getpid ()))

let fresh name =
  let d = Filename.concat base_dir name in
  mkdir_p d;
  (try
     Array.iter
       (fun f -> try Sys.remove (Filename.concat d f) with Sys_error _ -> ())
       (Sys.readdir d)
   with Sys_error _ -> ());
  d

(* A record with every interesting shape: a proper ULID-shaped run_id, a
   fractional occurrence that only %.17g round-trips, a `Failed verdict
   carrying a quoted string, and a log tail with escapes. *)
let record i : Spec.run =
  let verdict =
    match i mod 4 with
    | 0 -> `Ok
    | 1 -> `Failed "exit 2: dial tcp: timeout \"retry\""
    | 2 -> `Timeout
    | _ -> `Ok
  in
  { Spec.box = "box-7f3a";
    run_id = Printf.sprintf "01ARZ3NDEKTSV4RRFFQ69G5F%02d" (i mod 100);
    job = "acme-renew";
    occurrence = 1_760_000_000.123456 +. float_of_int i;
    started = 1_760_000_000.5 +. float_of_int i;
    finished = (if i mod 2 = 0 then Some (1_760_000_004.25 +. float_of_int i) else None);
    exit = (if i mod 3 = 0 then None else Some (i mod 7));
    signal = (if i mod 5 = 0 then Some 15 else None);
    verdict;
    attempt = 1 + (i mod 3);
    log_tail = Printf.sprintf "line\t%d\nquote \" backslash \\ ok" i }

let () =
  (* --- law: insert N, read back, assert equality field for field --------- *)
  let d = fresh "roundtrip" in
  let path = Filename.concat d "runs.db" in
  let n = 200 in
  let recs = List.init n record in
  (match Store.open_store ~path with
   | Error m -> incr failures; Printf.printf "FAIL open_store: %s\n" m
   | Ok t ->
     List.iter
       (fun r ->
          match Store.insert t r with
          | Ok () -> ()
          | Error m -> incr failures; Printf.printf "FAIL insert: %s\n" m)
       recs;
     (match Store.read_all t with
      | Error m -> incr failures; Printf.printf "FAIL read_all: %s\n" m
      | Ok back ->
        check (Printf.sprintf "roundtrip: %d in -> %d out" n (List.length back))
          (List.length back = List.length recs);
        let rec loop i rs bs =
          match rs, bs with
          | [], [] -> ()
          | r :: rs, b :: bs ->
            if not (r = b) then begin
              incr failures;
              Printf.printf
                "FAIL roundtrip row %d: run_id %S vs %S, occurrence %.17g vs \
                 %.17g\n"
                i r.Spec.run_id b.Spec.run_id r.Spec.occurrence
                b.Spec.occurrence
            end;
            loop (i + 1) rs bs
          | _ -> ()
        in
        loop 0 recs back;
        (* the fields the card names, asserted by hand as well *)
        match back with
        | [] -> ()
        | first :: _ ->
          check "roundtrip: run_id survives"
            (first.Spec.run_id = (List.hd recs).Spec.run_id);
          check "roundtrip: occurrence is bit-exact"
            (first.Spec.occurrence = (List.hd recs).Spec.occurrence);
          check "roundtrip: verdict carries its message"
            ((List.nth back 1).Spec.verdict =
             `Failed "exit 2: dial tcp: timeout \"retry\"");
          check "roundtrip: `Timeout stays `Timeout"
            ((List.nth back 2).Spec.verdict = `Timeout);
          check "roundtrip: finished None stays None"
            ((List.nth back 1).Spec.finished = None);
          check "roundtrip: log tail escapes survive"
            (has_sub first.Spec.log_tail "quote \" backslash \\ ok")));

  (* --- law: a reused store keeps history (append, not truncate) --------- *)
  (match Store.open_store ~path with
   | Error m -> incr failures; Printf.printf "FAIL reopen: %s\n" m
   | Ok t ->
     (match Store.read_all t with
      | Ok back ->
        check "reopen: history is still there" (List.length back = n)
      | Error m -> incr failures; Printf.printf "FAIL reread: %s\n" m);
     (match Store.insert t (record 0) with
      | Ok () -> ()
      | Error m -> incr failures; Printf.printf "FAIL insert after reopen: %s\n" m);
     (match Store.read_all t with
      | Ok back -> check "reopen: append adds exactly one" (List.length back = n + 1)
      | Error m -> incr failures; Printf.printf "FAIL reread2: %s\n" m));

  (* --- law: corrupt files are explicit errors, never crashes ------------ *)
  let corrupt name content =
    let d = fresh ("corrupt-" ^ name) in
    let path = Filename.concat d "runs.db" in
    write_file path content;
    match Store.open_store ~path with
    | Ok _ -> incr failures; Printf.printf "FAIL corrupt(%s): accepted\n" name
    | Error m ->
      check (Printf.sprintf "corrupt(%s): error names the file" name)
        (has_sub m path);
      Printf.printf "corrupt(%s) -> %s\n" name m
  in
  corrupt "empty" "";
  corrupt "foreign" "this is not an s-expression store\n";
  corrupt "no-header" "(run (box \"b\"))\n";
  corrupt "truncated"
    "(pp-cron-store 1)\n(run (box \"box-7f3a\") (run_id \"01ARZ\"";
  corrupt "bad-field"
    "(pp-cron-store 1)\n(run (box \"box-7f3a\") (run_id \"01ARZ\") (job \
     \"j\") (occurrence not-a-number) (started 1e9) (finished nil) (exit \
     nil) (signal nil) (verdict ok) (attempt 1) (log_tail \"\"))\n";
  corrupt "unknown-verdict"
    "(pp-cron-store 1)\n(run (box \"b\") (run_id \"r\") (job \"j\") \
     (occurrence 1) (started 2) (finished nil) (exit nil) (signal nil) \
     (verdict exploded) (attempt 1) (log_tail \"\"))\n";
  corrupt "binary" "\x00\x01\x02\x03garbage\xff";

  (* --- law: an unwritable path is an Error, not an exception ------------ *)
  (match Store.open_store ~path:"/nonexistent-pp-cron-dir/runs.db" with
   | Error m ->
     check "unwritable path: error mentions the path"
       (has_sub m "/nonexistent-pp-cron-dir/runs.db")
   | Ok _ -> incr failures; Printf.printf "FAIL unwritable path: accepted\n");

  (* a torn append on a valid store is refused too, and says where *)
  let d = fresh "torn" in
  let path = Filename.concat d "runs.db" in
  (match Store.open_store ~path with
   | Error m -> incr failures; Printf.printf "FAIL torn setup: %s\n" m
   | Ok t ->
     (match Store.insert t (record 1) with
      | Ok () -> ()
      | Error m -> incr failures; Printf.printf "FAIL torn insert: %s\n" m));
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let keep = max 0 (len - 12) in
  let src = really_input_string ic keep in
  close_in ic;
  write_file path src;
  (match Store.open_store ~path with
   | Ok _ -> incr failures; Printf.printf "FAIL torn: accepted\n"
   | Error m ->
     check "torn: error names a line" (has_sub m ":");
     check "torn: error names the file" (has_sub m path));

  if !failures = 0 then print_string "store_test: all laws hold\n"
  else begin
    Printf.printf "store_test: %d failure(s)\n" !failures;
    exit 1
  end
