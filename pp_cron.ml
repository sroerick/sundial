(* pp-cron command line. S1 (card a88d99) shipped `check`: the record,
   the schedule grammar and its validation. Card e1cf5e32 adds central's
   half: `ingest` verifies the box's evidence before anything is stored,
   and `verify` recomputes it over the stored records, naming the run_id
   of any record altered at rest. No scheduler, no executor. *)

let usage =
  "usage: pp-cron check [<jobs-dir>...]\n\
  \       pp-cron ingest <store-dir> <record-file>...\n\
  \       pp-cron verify <store-dir>\n\
  \  (default jobs directory: /etc/pp-cron/jobs)"

let () =
  match Array.to_list Sys.argv with
  | _ :: "check" :: rest ->
    let dirs = match rest with [] -> [ "/etc/pp-cron/jobs" ] | l -> l in
    let bad = ref false in
    List.iter
      (fun dir ->
         match Spec.check_dir dir with
         | Ok jobs ->
           Printf.printf "%s: %d job(s) ok\n" dir (List.length jobs)
         | Error errs ->
           bad := true;
           List.iter (fun e -> prerr_endline ("pp-cron: " ^ e)) errs)
      dirs;
    exit (if !bad then 1 else 0)
  | _ :: "ingest" :: dir :: records ->
    let bad = ref false in
    List.iter
      (fun file ->
         match Spec.read_record file with
         | Error m ->
           bad := true;
           prerr_endline ("pp-cron: REJECTED record: " ^ m)
         | Ok r -> (
           match Spec.ingest dir r with
           | Ok () -> Printf.printf "stored run %s\n" r.Spec.run_id
           | Error _ -> bad := true))
      records;
    exit (if !bad then 1 else 0)
  | _ :: "verify" :: [ dir ] -> (
    match Spec.verify_store dir with
    | [] ->
      Printf.printf "%s: store ok, evidence matches\n" dir;
      exit 0
    | errs ->
      List.iter (fun e -> prerr_endline ("pp-cron: " ^ e)) errs;
      exit 1)
  | _ :: ("run" | "status" | "serve") :: _ ->
    prerr_endline "pp-cron: not implemented in this card (record + check only)";
    exit 2
  | _ ->
    prerr_endline usage;
    exit 2
