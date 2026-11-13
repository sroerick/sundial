(* pp-cron command line. S1 (card a88d99) ships `check`: the record, the
   schedule grammar and its validation. S3a (card d1b6ba) links the
   durable run-record store (store.ml), exercised by store_test. *)

let usage =
  "usage: pp-cron check [<jobs-dir>...]\n\
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
  | _ :: ("run" | "status" | "serve") :: _ ->
    prerr_endline "pp-cron: not implemented in this card (record + check only)";
    exit 2
  | _ ->
    prerr_endline usage;
    exit 2
