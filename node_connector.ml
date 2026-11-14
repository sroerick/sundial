(* pp-node: the node connector (card 43a6c99d, P1 webhook + P2 wireguard).

   ONE doorbell shape town-wide: the same sender-key HMAC pattern as tuna's
   webhook card 35b5f79c. The receiver VERIFIES and WRITES, it never acts.
   A signed POST becomes an [event.webhook] claim in the journal carrying
   [(action none)]; an unsigned or mis-signed POST is a 401 and writes
   nothing at all.

   The wireguard administrator is a static-mesh KEY AUTHORITY, not a
   coordination server: peer configs are PRIVATE-scope claims under
   <store>/private/peers/. The root signing key lives only in the daemon
   environment (PP_NODE_ROOT_KEY) and is never written into the store. *)

let ( +% ) = Int32.add
let ( ^% ) = Int32.logxor
let ( &% ) = Int32.logand

let rotr x n =
  Int32.logor (Int32.shift_right_logical x n) (Int32.shift_left x (32 - n))

(* ------------------------------------------------------------------ *)
(* SHA-256 and HMAC-SHA-256: no external crypto, no network            *)
(* ------------------------------------------------------------------ *)

let k =
  [| 0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l;
     0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
     0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l;
     0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
     0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl;
     0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
     0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l;
     0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
     0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l;
     0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
     0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l;
     0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
     0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l;
     0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
     0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l;
     0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l |]

let sha256 (msg : string) : string =
  let len = String.length msg in
  let bitlen = Int64.mul (Int64.of_int len) 8L in
  let blocks = (len + 9 + 63) / 64 in
  let total = blocks * 64 in
  let buf = Bytes.make total '\000' in
  Bytes.blit_string msg 0 buf 0 len;
  Bytes.set buf len '\x80';
  for i = 0 to 7 do
    let b =
      Int64.logand (Int64.shift_right_logical bitlen (8 * (7 - i))) 0xffL
    in
    Bytes.set buf (total - 8 + i) (Char.chr (Int64.to_int b))
  done;
  let h =
    [| 0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al;
       0x510e527fl; 0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l |]
  in
  let w = Array.make 64 0l in
  for block = 0 to blocks - 1 do
    for t = 0 to 15 do
      let base = (block * 64) + (t * 4) in
      let byte i = Int32.of_int (Char.code (Bytes.get buf (base + i))) in
      w.(t) <-
        Int32.logor (Int32.shift_left (byte 0) 24)
          (Int32.logor (Int32.shift_left (byte 1) 16)
             (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3)))
    done;
    for t = 16 to 63 do
      let x = w.(t - 15) and y = w.(t - 2) in
      let s0 = rotr x 7 ^% rotr x 18 ^% Int32.shift_right_logical x 3 in
      let s1 = rotr y 17 ^% rotr y 19 ^% Int32.shift_right_logical y 10 in
      w.(t) <- w.(t - 16) +% s0 +% w.(t - 7) +% s1
    done;
    let a = ref h.(0) and b = ref h.(1) and c = ref h.(2) and d = ref h.(3) in
    let e = ref h.(4) and f = ref h.(5) and g = ref h.(6) and hh = ref h.(7) in
    for t = 0 to 63 do
      let s1 = rotr !e 6 ^% rotr !e 11 ^% rotr !e 25 in
      let ch = (!e &% !f) ^% (Int32.lognot !e &% !g) in
      let t1 = !hh +% s1 +% ch +% k.(t) +% w.(t) in
      let s0 = rotr !a 2 ^% rotr !a 13 ^% rotr !a 22 in
      let maj = (!a &% !b) ^% (!a &% !c) ^% (!b &% !c) in
      let t2 = s0 +% maj in
      hh := !g;
      g := !f;
      f := !e;
      e := !d +% t1;
      d := !c;
      c := !b;
      b := !a;
      a := t1 +% t2
    done;
    h.(0) <- h.(0) +% !a;
    h.(1) <- h.(1) +% !b;
    h.(2) <- h.(2) +% !c;
    h.(3) <- h.(3) +% !d;
    h.(4) <- h.(4) +% !e;
    h.(5) <- h.(5) +% !f;
    h.(6) <- h.(6) +% !g;
    h.(7) <- h.(7) +% !hh
  done;
  let out = Buffer.create 32 in
  Array.iter
    (fun x ->
       for i = 3 downto 0 do
         let byte =
           Int32.logand (Int32.shift_right_logical x (8 * i)) 0xffl
         in
         Buffer.add_char out (Char.chr (Int32.to_int byte))
       done)
    h;
  Buffer.contents out

let hex_of_string s =
  let b = Buffer.create (String.length s * 2) in
  String.iter
    (fun c -> Buffer.add_string b (Printf.sprintf "%02x" (Char.code c)))
    s;
  Buffer.contents b

let hmac_raw ~key ~msg =
  let block = 64 in
  let key = if String.length key > block then sha256 key else key in
  let key = key ^ String.make (block - String.length key) '\000' in
  let pad c = String.map (fun x -> Char.chr (Char.code x lxor c)) key in
  sha256 (pad 0x5c ^ sha256 (pad 0x36 ^ msg))

let hmac_sha256 ~key ~msg = hex_of_string (hmac_raw ~key ~msg)

let strip_prefix p s =
  let n = String.length p in
  if String.length s >= n && String.sub s 0 n = p then
    String.sub s n (String.length s - n)
  else s

(* A signature is hex, optionally prefixed sha256= (the sender-key shape). *)
let signature_valid ~key ~body ~signature =
  let provided = String.lowercase_ascii (strip_prefix "sha256=" signature) in
  provided <> "" && provided = hmac_sha256 ~key ~msg:body

(* ------------------------------------------------------------------ *)
(* The journal: verification output only, never an action              *)
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

(* The one doorbell shape (35b5f79c): the claim says what arrived and says
   explicitly that ingress did nothing with it. *)
let webhook_event_claim ~body ~sig_hex =
  Printf.sprintf
    "(claim (kind event.webhook) (scope public) (door 35b5f79c) (action none) \
     (sig %s) (body \"%s\"))"
    (esc sig_hex) (esc body)

let rec mkdir_p dir =
  if dir = "" || dir = "/" || dir = "." then ()
  else if Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path s =
  let oc =
    open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] 0o600 path
  in
  output_string oc s;
  if s = "" || s.[String.length s - 1] <> '\n' then output_char oc '\n';
  close_out oc

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let append_line path s =
  mkdir_p (Filename.dirname path);
  let oc = open_out_gen [ Open_wronly; Open_creat; Open_append; Open_binary ] 0o600 path in
  output_string oc s;
  output_char oc '\n';
  (try Unix.fsync (Unix.descr_of_out_channel oc) with Unix.Unix_error _ -> ());
  close_out oc

(* ------------------------------------------------------------------ *)
(* The mesh key authority: tiers, peer claims, rotation                *)
(* ------------------------------------------------------------------ *)

let tiers = [ "host"; "relay"; "exit" ]

let rank = function "host" -> 1 | "relay" -> 2 | "exit" -> 3 | _ -> 0

let valid_tier t = List.mem t tiers

let above_tier ~tier ~max_tier = rank tier > rank max_tier

let peers_dir store = Filename.concat (Filename.concat store "private") "peers"

(* A peer key is 32 random bytes, written base64 - never raw. A raw key
   carries arbitrary bytes and no s-expression could read the claim back:
   the claim must round-trip through its own grammar. *)
let b64_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let b64 (s : string) : string =
  let n = String.length s in
  let b = Buffer.create ((n + 2) / 3 * 4) in
  let get i = if i < n then Char.code s.[i] else 0 in
  let rec go i =
    if i < n then begin
      let x = get i and y = get (i + 1) and z = get (i + 2) in
      Buffer.add_char b b64_alphabet.[x lsr 2];
      Buffer.add_char b b64_alphabet.[((x land 3) lsl 4) lor (y lsr 4)];
      if i + 1 < n then
        Buffer.add_char b b64_alphabet.[((y land 15) lsl 2) lor (z lsr 6)]
      else Buffer.add_char b '=';
      if i + 2 < n then Buffer.add_char b b64_alphabet.[z land 63]
      else Buffer.add_char b '=';
      go (i + 3)
    end
  in
  go 0;
  Buffer.contents b

let fresh_key () =
  let seed =
    Hashtbl.hash (Unix.gettimeofday (), Unix.getpid (), Random.bits ())
  in
  let st = Random.State.make [| seed |] in
  b64 (String.init 32 (fun _ -> Char.chr (Random.State.int st 256)))

let derive_pub priv = hex_of_string (sha256 ("pp-node-wg-pub:" ^ priv))

let canonical ~name ~tier ~addr ~epoch ~pub =
  Printf.sprintf "%s|%s|%s|%d|%s" name tier addr epoch pub

let peer_claim ~name ~tier ~addr ~epoch ~priv ~root =
  let pub = derive_pub priv in
  let sig_ = hmac_sha256 ~key:root ~msg:(canonical ~name ~tier ~addr ~epoch ~pub) in
  Printf.sprintf
    "(claim (kind peer) (name %s) (scope private) (tier %s) (addr %s) \
     (epoch %d) (priv %s) (pub %s) (sig %s))"
    (esc name) (esc tier) (esc addr) epoch (esc priv) (esc pub) (esc sig_)

let peer_conf ~name ~addr ~priv ~claim =
  let pub = derive_pub priv in
  String.concat "\n"
    [ Printf.sprintf "# pp-node peer %s (PRIVATE scope, claim-signed)" name;
      "[Interface]";
      Printf.sprintf "Address = %s/32" addr;
      Printf.sprintf "PrivateKey = %s" priv;
      "Table = off";
      "[Peer]";
      Printf.sprintf "PublicKey = %s" pub;
      "AllowedIPs = 10.90.0.0/24";
      Printf.sprintf "# claim: %s" claim;
      "" ]

(* A minimal s-expression reader for claim files: atoms, quoted strings and
   lists, comments skipped. Local on purpose - a claim is the only grammar
   this module reads, and it must not drag the job-file schema in. *)
type sexp = Atom of string | List of sexp list

let parse_sexps (src : string) : sexp list =
  let n = String.length src in
  let i = ref 0 in
  let peek () = if !i < n then Some src.[!i] else None in
  let rec skip () =
    match peek () with
    | Some (' ' | '\t' | '\r' | '\n') -> incr i; skip ()
    | Some ';' ->
      while peek () <> None && peek () <> Some '\n' do incr i done;
      skip ()
    | _ -> ()
  in
  let parse_string () =
    incr i;
    let b = Buffer.create 16 in
    let fin = ref false in
    while not !fin do
      match peek () with
      | None -> fin := true
      | Some '"' -> incr i; fin := true
      | Some '\\' -> (
        incr i;
        match peek () with
        | None -> fin := true
        | Some c ->
          Buffer.add_char b
            (match c with 'n' -> '\n' | 't' -> '\t' | 'r' -> '\r' | c -> c);
          incr i)
      | Some c -> Buffer.add_char b c; incr i
    done;
    Atom (Buffer.contents b)
  in
  let parse_atom () =
    let b = Buffer.create 16 in
    let fin = ref false in
    while not !fin do
      match peek () with
      | Some c
        when c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n' && c <> '('
             && c <> ')' && c <> ';' && c <> '"' ->
        Buffer.add_char b c;
        incr i
      | _ -> fin := true
    done;
    Atom (Buffer.contents b)
  in
  let rec parse_one () =
    skip ();
    match peek () with
    | None -> Atom ""
    | Some '(' ->
      incr i;
      let items = ref [] in
      let fin = ref false in
      while not !fin do
        skip ();
        match peek () with
        | None -> fin := true
        | Some ')' -> incr i; fin := true
        | Some _ -> items := parse_one () :: !items
      done;
      List (List.rev !items)
    | Some ')' -> incr i; Atom ""
    | Some '"' -> parse_string ()
    | Some _ -> parse_atom ()
  in
  let out = ref [] in
  let fin = ref false in
  while not !fin do
    skip ();
    match peek () with
    | None -> fin := true
    | Some _ -> out := parse_one () :: !out
  done;
  List.rev !out

let peer_fields (src : string) : ((string * string) list, string) result =
  match parse_sexps src with
  | [ List (Atom "claim" :: items) ] ->
    let out = ref [] in
    List.iter
      (function
        | List [ Atom key; Atom v ] -> out := (key, v) :: !out
        | _ -> ())
      items;
    Ok (List.rev !out)
  | _ -> Error "expected exactly one (claim ...) form"

let verify_claim ~root src =
  match peer_fields src with
  | Error _ -> false
  | Ok f -> (
    let g key = List.assoc_opt key f in
    match
      ( g "kind", g "name", g "tier", g "addr", g "epoch", g "pub", g "sig" )
    with
    | Some "peer", Some name, Some tier, Some addr, Some epoch, Some pub,
      Some sig_ -> (
      match int_of_string_opt epoch with
      | None -> false
      | Some epoch ->
        hmac_sha256 ~key:root
          ~msg:(canonical ~name ~tier ~addr ~epoch ~pub)
        = sig_)
    | _ -> false)
