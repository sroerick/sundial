(* pp-cron local store (card S3a): the durable run-record store.

   This replaces the S2 jsonl stub. One append-only file of s-expressions,
   fsync'd per insert, so a terminal run row is on disk before the caller
   returns. The grammar is read with [Spec.parse_sexp] - the same parser the
   job files use - and every failure is an explicit [Error] naming the file
   and the line: a torn append, a hand-edit or a foreign file is reported,
   never raised. *)

type t = { path : string }

exception Bad of string

let header = "(pp-cron-store 1)"

let bad fmt = Printf.ksprintf (fun m -> raise (Bad m)) fmt

(* ------------------------------------------------------------------ *)
(* Writing: one s-expression per run, appended and fsync'd             *)
(* ------------------------------------------------------------------ *)

let esc s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
       match c with
       | '\\' -> Buffer.add_string b "\\\\"
       | '"' -> Buffer.add_string b "\\\""
       | '\n' -> Buffer.add_string b "\\n"
       | '\t' -> Buffer.add_string b "\\t"
       | '\r' -> Buffer.add_string b "\\r"
       | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* %.17g round-trips an IEEE double exactly, so occurrence and started
   compare equal after a write/read cycle. *)
let float_s f = Printf.sprintf "%.17g" f

let verdict_s = function
  | `Ok -> "ok"
  | `Timeout -> "timeout"
  | `Missing -> "missing"
  | `Failed r -> Printf.sprintf "(failed \"%s\")" (esc r)

let sexp_of_run (r : Spec.run) =
  let opt f = function None -> "nil" | Some v -> f v in
  Printf.sprintf
    "(run (box \"%s\") (run_id \"%s\") (job \"%s\") (occurrence %s) \
     (started %s) (finished %s) (exit %s) (signal %s) (verdict %s) \
     (attempt %d) (log_tail \"%s\"))"
    (esc r.box) (esc r.run_id) (esc r.job) (float_s r.occurrence)
    (float_s r.started) (opt float_s r.finished) (opt string_of_int r.exit)
    (opt string_of_int r.signal) (verdict_s r.verdict) r.attempt
    (esc r.log_tail)

(* ------------------------------------------------------------------ *)
(* Reading: the same grammar, refused explicitly when it does not fit  *)
(* ------------------------------------------------------------------ *)

let atom = function
  | Spec.Atom (s, _) -> s
  | Spec.List (_, l) -> bad "line %d: expected an atom" l

let parse_run l (items : Spec.sexp list) : Spec.run =
  let fields = Hashtbl.create 17 in
  List.iter
    (fun it ->
       match it with
       | Spec.List ([ Spec.Atom (k, _); v ], _) -> Hashtbl.replace fields k v
       | Spec.List (_, ln) -> bad "line %d: malformed run field" ln
       | Spec.Atom (a, ln) -> bad "line %d: unexpected atom %S in run" ln a)
    items;
  let get k =
    match Hashtbl.find_opt fields k with
    | Some v -> v
    | None -> bad "line %d: run is missing (%s ...)" l k
  in
  let num k =
    match float_of_string_opt (atom (get k)) with
    | Some v -> v
    | None -> bad "line %d: (%s) is not a number" l k
  in
  let num_opt k =
    match get k with
    | Spec.Atom ("nil", _) -> None
    | v -> (
      match float_of_string_opt (atom v) with
      | Some f -> Some f
      | None -> bad "line %d: (%s) is not a number" l k)
  in
  let int_opt k =
    match get k with
    | Spec.Atom ("nil", _) -> None
    | v -> (
      match int_of_string_opt (atom v) with
      | Some i -> Some i
      | None -> bad "line %d: (%s) is not an integer" l k)
  in
  let int k =
    match int_of_string_opt (atom (get k)) with
    | Some i -> i
    | None -> bad "line %d: (%s) is not an integer" l k
  in
  let verdict =
    match get "verdict" with
    | Spec.Atom ("ok", _) -> `Ok
    | Spec.Atom ("timeout", _) -> `Timeout
    | Spec.Atom ("missing", _) -> `Missing
    | Spec.List ([ Spec.Atom ("failed", _); v ], _) -> `Failed (atom v)
    | Spec.List (_, ln) -> bad "line %d: malformed verdict" ln
    | Spec.Atom (s, ln) -> bad "line %d: unknown verdict %S" ln s
  in
  { Spec.box = atom (get "box");
    run_id = atom (get "run_id");
    job = atom (get "job");
    occurrence = num "occurrence";
    started = num "started";
    finished = num_opt "finished";
    exit = int_opt "exit";
    signal = int_opt "signal";
    verdict;
    attempt = int "attempt";
    log_tail = atom (get "log_tail") }

let run_of_sexp = function
  | Spec.List (Spec.Atom ("run", _) :: items, l) -> parse_run l items
  | Spec.List (_, l) -> bad "line %d: expected (run ...)" l
  | Spec.Atom (_, l) -> bad "line %d: expected a list" l

let parse_store path src : Spec.run list =
  match Spec.parse_sexp path src with
  | Spec.List
      (Spec.Atom ("pp-cron-store", _) :: Spec.Atom ("1", _) :: [], _) :: rest ->
    List.map run_of_sexp rest
  | _ -> bad "missing %s header" header

let read_string path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let fsync_out oc =
  try Unix.fsync (Unix.descr_of_out_channel oc) with Unix.Unix_error _ -> ()

(* ------------------------------------------------------------------ *)
(* The interface                                                       *)
(* ------------------------------------------------------------------ *)

let open_store ~path : (t, string) result =
  if not (Sys.file_exists path) then
    try
      let oc = open_out_bin path in
      output_string oc (header ^ "\n");
      flush oc;
      fsync_out oc;
      close_out oc;
      Ok { path }
    with Sys_error m -> Error m
  else
    try
      let src = read_string path in
      if String.length src = 0 then bad "empty store file: %s header missing" header;
      ignore (parse_store path src);
      Ok { path }
    with
    | Spec.Refused m -> Error m
    | Bad m -> Error (Printf.sprintf "%s: %s" path m)
    | Sys_error m -> Error m

let insert t (r : Spec.run) : (unit, string) result =
  try
    let oc = open_out_gen [ Open_append; Open_binary ] 0o600 t.path in
    output_string oc (sexp_of_run r ^ "\n");
    flush oc;
    fsync_out oc;
    close_out oc;
    Ok ()
  with
  | Sys_error m -> Error m
  | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)

let read_all t : (Spec.run list, string) result =
  try Ok (parse_store t.path (read_string t.path)) with
  | Spec.Refused m -> Error m
  | Bad m -> Error (Printf.sprintf "%s: %s" t.path m)
  | Sys_error m -> Error m
