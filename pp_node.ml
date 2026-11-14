(* pp-node command line: the two doors of the node connector (card 43a6c99d).

   Door one is the doorbell: [receive] verifies a sender-key HMAC (tuna's
   card 35b5f79c shape, ONE shape town-wide), writes an [event.webhook]
   claim into the journal and DOES NOTHING ELSE - the cron reacts, ingress
   stays dumb and auditable.

   Door two is the mesh key authority: [peer mint|rotate|heartbeat|verify]
   stores peer configs as PRIVATE-scope claims under <store>/private/peers/,
   deploys <hostname>.wg0, and audits/rotates on schedule. The root signing
   key comes from the environment ($PP_NODE_ROOT_KEY) and is never written
   into the store. *)

let usage =
  String.concat "\n"
    [ "usage: pp-node receive --store <dir> --key <sender-key> \
       [--signature <sig>]   (body is stdin)";
      "       pp-node sign --key <sender-key>   (stdin -> hex signature)";
      "       pp-node peer mint --store <dir> --name <n> --tier <host|relay|exit> \
       --addr <ip> --epoch <n> --max-tier <host|relay|exit> [--wg-dir <dir>]";
      "       pp-node peer rotate --store <dir> --name <n> --addr <ip> \
       --epoch <n> [--wg-dir <dir>]";
      "       pp-node peer heartbeat --store <dir> --name <n> --epoch <n>";
      "       pp-node peer verify --store <dir> --name <n>";
      "       (the root signing key is $PP_NODE_ROOT_KEY, never stored)" ]

let die code fmt =
  Printf.ksprintf (fun m -> prerr_endline ("pp-node: " ^ m); exit code) fmt

(* The body of a POST is stdin: read it whole, never line by line, so the
   HMAC covers exactly the bytes the sender signed. *)
let read_stdin () =
  let b = Buffer.create 256 in
  let chunk = Bytes.create 4096 in
  let rec go () =
    let n = input stdin chunk 0 (Bytes.length chunk) in
    if n > 0 then begin
      Buffer.add_subbytes b chunk 0 n;
      go ()
    end
  in
  (try go () with End_of_file -> ());
  Buffer.contents b

let opts argv =
  let rec go acc = function
    | [] -> List.rev acc
    | k :: v :: rest
      when String.length k > 2 && String.sub k 0 2 = "--" ->
      go ((String.sub k 2 (String.length k - 2), v) :: acc) rest
    | k :: _ -> die 2 "expected --flag value, got %S" k
  in
  go [] argv

let get args name =
  match List.assoc_opt name args with
  | Some v -> v
  | None -> die 2 "missing --%s" name

let int_arg args name =
  let v = get args name in
  match int_of_string_opt v with
  | Some n -> n
  | None -> die 2 "--%s: expected an integer, got %S" name v

let root_key () =
  match Sys.getenv_opt "PP_NODE_ROOT_KEY" with
  | Some k when k <> "" -> k
  | _ -> die 2 "PP_NODE_ROOT_KEY is not set (the root key is never stored)"

let journal store = Filename.concat store "journal"

let peer_path store name =
  Filename.concat (Node_connector.peers_dir store) (name ^ ".claim")

let wg_dir store args =
  match List.assoc_opt "wg-dir" args with
  | Some d -> d
  | None -> Filename.concat store "wg"

(* The factory deploys this file; the daemon only ever writes it. *)
let deploy ~store ~args ~name ~addr ~priv ~claim =
  let dir = wg_dir store args in
  Node_connector.mkdir_p dir;
  let path = Filename.concat dir (Unix.gethostname () ^ ".wg0") in
  Node_connector.write_file path
    (Node_connector.peer_conf ~name ~addr ~priv ~claim)

let read_claim ~store ~name =
  try Node_connector.read_file (peer_path store name)
  with Sys_error m -> die 1 "peer %s: no such claim: %s" name m

(* ------------------------------------------------------------------ *)
(* Door one: the doorbell                                              *)
(* ------------------------------------------------------------------ *)

let receive args =
  let store = get args "store" in
  let key = get args "key" in
  let signature =
    match List.assoc_opt "signature" args with Some s -> s | None -> ""
  in
  let body = read_stdin () in
  if Node_connector.signature_valid ~key ~body ~signature then begin
    Node_connector.append_line (journal store)
      (Node_connector.webhook_event_claim ~body ~sig_hex:signature);
    print_endline "200 ok (event claim written; ingress acted on nothing)";
    0
  end
  else begin
    prerr_endline "401 unauthorized (unsigned or mis-signed; nothing written)";
    1
  end

(* ------------------------------------------------------------------ *)
(* Door two: the mesh key authority                                    *)
(* ------------------------------------------------------------------ *)

let mint args =
  let store = get args "store" in
  let name = get args "name" in
  let tier = get args "tier" in
  let addr = get args "addr" in
  let epoch = int_arg args "epoch" in
  let max_tier =
    match List.assoc_opt "max-tier" args with Some t -> t | None -> tier
  in
  if not (Node_connector.valid_tier tier) then
    die 2 "unknown tier %S (host | relay | exit)" tier;
  if not (Node_connector.valid_tier max_tier) then
    die 2 "unknown --max-tier %S (host | relay | exit)" max_tier;
  if Node_connector.above_tier ~tier ~max_tier then begin
    prerr_endline
      (Printf.sprintf
         "pp-node: refused: tier %s is above this node's allowance %s; \
          nothing written"
         tier max_tier);
    3
  end
  else begin
    let root = root_key () in
    let priv = Node_connector.fresh_key () in
    let claim = Node_connector.peer_claim ~name ~tier ~addr ~epoch ~priv ~root in
    let path = peer_path store name in
    Node_connector.mkdir_p (Filename.dirname path);
    Node_connector.write_file path claim;
    deploy ~store ~args ~name ~addr ~priv ~claim;
    (* A mint is not trusted until the node hears it: heartbeat confirms. *)
    Node_connector.append_line (journal store)
      (Printf.sprintf
         "(claim (kind peer.heartbeat) (name %s) (epoch %d) (ok true))" name
         epoch);
    print_endline claim;
    0
  end

let rotate args =
  let store = get args "store" in
  let name = get args "name" in
  let addr = get args "addr" in
  let epoch = int_arg args "epoch" in
  let old = read_claim ~store ~name in
  let tier =
    match Node_connector.peer_fields old with
    | Ok f -> (
      match List.assoc_opt "tier" f with
      | Some t -> t
      | None -> die 1 "peer %s: claim names no tier" name)
    | Error m -> die 1 "peer %s: %s" name m
  in
  if not (Node_connector.verify_claim ~root:(root_key ()) old) then
    die 1 "peer %s: current claim does not verify; rotation refused" name;
  let root = root_key () in
  let priv = Node_connector.fresh_key () in
  let claim = Node_connector.peer_claim ~name ~tier ~addr ~epoch ~priv ~root in
  let path = peer_path store name in
  Node_connector.write_file path claim;
  deploy ~store ~args ~name ~addr ~priv ~claim;
  Node_connector.append_line (journal store)
    (Printf.sprintf
       "(claim (kind peer.rotation) (name %s) (epoch %d) (ok true))" name epoch);
  print_endline claim;
  0

let heartbeat args =
  let store = get args "store" in
  let name = get args "name" in
  let epoch = int_arg args "epoch" in
  let src = read_claim ~store ~name in
  if not (Node_connector.verify_claim ~root:(root_key ()) src) then
    die 1 "peer %s: claim does not verify" name;
  Node_connector.append_line (journal store)
    (Printf.sprintf
       "(claim (kind peer.heartbeat) (name %s) (epoch %d) (ok true))" name epoch);
  print_endline ("peer " ^ name ^ " heartbeat ok");
  0

let verify args =
  let store = get args "store" in
  let name = get args "name" in
  let src = read_claim ~store ~name in
  if Node_connector.verify_claim ~root:(root_key ()) src then begin
    print_endline ("peer " ^ name ^ " verified");
    0
  end
  else begin
    prerr_endline ("peer " ^ name ^ ": claim does not verify");
    1
  end

(* ------------------------------------------------------------------ *)

let () =
  let code =
    match Array.to_list Sys.argv with
    | _ :: "receive" :: rest -> receive (opts rest)
    | _ :: "sign" :: rest ->
      let key = get (opts rest) "key" in
      print_endline (Node_connector.hmac_sha256 ~key ~msg:(read_stdin ()));
      0
    | _ :: "peer" :: "mint" :: rest -> mint (opts rest)
    | _ :: "peer" :: "rotate" :: rest -> rotate (opts rest)
    | _ :: "peer" :: "heartbeat" :: rest -> heartbeat (opts rest)
    | _ :: "peer" :: "verify" :: rest -> verify (opts rest)
    | _ -> prerr_endline usage; 2
  in
  exit code
