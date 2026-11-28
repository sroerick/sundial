(* pp-cron env: per-job KEY=VALUE files and the minimal child environment.

   Values are the bytes after the first '=': no shell expansion, no quoting.
   The RUN_ID contract is appended last and cannot be overridden by a file. *)

type binding = string * string

let contract_keys =
  [ "RUN_ID"; "PP_CRON_JOB"; "PP_CRON_OCCURRENCE"; "PP_CRON_ATTEMPT"; "PP_CRON_BOX" ]

let is_key k =
  k <> ""
  && not (k.[0] >= '0' && k.[0] <= '9')
  && String.for_all
       (fun c ->
         (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
         || (c >= '0' && c <= '9') || c = '_')
       k

let strip_cr s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

let parse ~file (src : string) : (binding list, string) result =
  let rec go n acc = function
    | [] -> Ok (List.rev acc)
    | line :: rest ->
      let line = strip_cr line in
      let trimmed = String.trim line in
      if trimmed = "" || trimmed.[0] = '#' then go (n + 1) acc rest
      else (
        match String.index_opt line '=' with
        | None -> Error (Printf.sprintf "%s:%d: expected KEY=VALUE" file n)
        | Some k ->
          let key = String.sub line 0 k in
          let value = String.sub line (k + 1) (String.length line - k - 1) in
          if not (is_key key) then
            Error (Printf.sprintf "%s:%d: %S is not a KEY" file n key)
          else go (n + 1) ((key, value) :: acc) rest)
  in
  go 1 [] (String.split_on_char '\n' src)

(* Read once at job load; a bad line refuses the whole file. *)
let load path : (binding list, string) result =
  match Spec.read_file path with
  | Error m -> Error (Printf.sprintf "%s:1: %s" path m)
  | Ok src -> parse ~file:path src

let split_env s =
  match String.index_opt s '=' with
  | None -> (s, "")
  | Some k -> (String.sub s 0 k, String.sub s (k + 1) (String.length s - k - 1))

let daemon_value daemon key =
  List.find_map
    (fun s ->
      let k, v = split_env s in
      if k = key then Some v else None)
    (Array.to_list daemon)

(* PATH, HOME and the job's tz are inherited minimal; [base] is the job's env
   file; the RUN_ID contract comes from the record the box just minted. *)
let child_env ?(daemon = Unix.environment ()) ?(base : binding list = [])
    (job : Spec.job) (start : Spec.run_start) : binding list =
  let file =
    List.filter (fun (k, _) -> not (List.mem k contract_keys) && k <> "TZ") base
  in
  let inherited =
    [ ("PATH", Option.value (daemon_value daemon "PATH") ~default:"/usr/bin:/bin");
      ("HOME", Option.value (daemon_value daemon "HOME") ~default:"/nonexistent") ]
  in
  let inherited = List.filter (fun (k, _) -> not (List.mem_assoc k file)) inherited in
  let contract =
    [ ("RUN_ID", start.Spec.run_id);
      ("PP_CRON_JOB", job.Spec.name);
      ("PP_CRON_OCCURRENCE", Printf.sprintf "%.0f" start.Spec.occurrence);
      ("PP_CRON_ATTEMPT", string_of_int start.Spec.attempt);
      ("PP_CRON_BOX", start.Spec.box) ]
  in
  inherited @ file @ [ ("TZ", job.Spec.tz) ] @ contract
