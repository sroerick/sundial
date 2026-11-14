#!/bin/sh
# card 43a6c99d (P2): the mesh key authority. A mint lands a PRIVATE-scope
# claim, deploys <hostname>.wg0 on the node, the heartbeat confirms it, a
# rotation round-trips (new key, same tier, verifies), and the root signing
# key is never written into the store.
set -eu

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)
if [ -x "$root/pp_node.exe" ]; then
  bin=${PP_NODE:-$root/pp_node.exe}
else
  bin=${PP_NODE:-$root/_build/default/pp_node.exe}
fi

[ -x "$bin" ] || { echo "peer_mint: no pp_node at $bin" >&2; exit 1; }

fail() { echo "peer_mint: $*" >&2; exit 1; }

store=$(mktemp -d "${TMPDIR:-/tmp}/pp-node-mint.XXXXXX")
wg=$(mktemp -d "${TMPDIR:-/tmp}/pp-node-mint-wg.XXXXXX")
trap 'rm -rf "$store" "$wg"' EXIT

rootkey=root-secret-0123456789abcdef
PP_NODE_ROOT_KEY=$rootkey
export PP_NODE_ROOT_KEY

claim=$store/private/peers/edge1.claim
conf=$wg/$(hostname).wg0

# ---- mint ---------------------------------------------------------------
"$bin" peer mint --store "$store" --name edge1 --tier relay --addr 10.90.0.5 \
  --epoch 1 --max-tier exit --wg-dir "$wg" >/dev/null || fail "mint failed"

[ -f "$claim" ] || fail "mint wrote no claim"
grep -q '(kind peer)' "$claim" || fail "claim is not a peer claim"
grep -q '(scope private)' "$claim" || fail "claim is not PRIVATE scope"
case $claim in
  "$store"/private/peers/*) : ;;
  *) fail "claim is not under private/peers/" ;;
esac
grep -q '(tier relay)' "$claim" || fail "claim lost its tier"

[ -f "$conf" ] || fail "no $(basename "$conf") deployed on the node"
grep -q 'PrivateKey' "$conf" || fail "conf carries no key"

# ---- heartbeat confirms -------------------------------------------------
"$bin" peer heartbeat --store "$store" --name edge1 --epoch 1 >/dev/null \
  || fail "heartbeat did not confirm the mint"
grep -q '(kind peer.heartbeat)' "$store/journal" || fail "no heartbeat claim"
"$bin" peer verify --store "$store" --name edge1 >/dev/null \
  || fail "minted claim does not verify"

# ---- rotation round-trips -----------------------------------------------
before=$(sed -n 's/.*(priv \([^)]*\)).*/\1/p' "$claim")
"$bin" peer rotate --store "$store" --name edge1 --addr 10.90.0.5 \
  --epoch 2 --wg-dir "$wg" >/dev/null || fail "rotation failed"
after=$(sed -n 's/.*(priv \([^)]*\)).*/\1/p' "$claim")

[ -n "$before" ] && [ -n "$after" ] || fail "claim carries no private key"
[ "$before" != "$after" ] || fail "rotation reused the old key"
grep -q '(tier relay)' "$claim" || fail "rotation changed the tier"
grep -q '(epoch 2)' "$claim" || fail "rotation did not move the epoch"
"$bin" peer verify --store "$store" --name edge1 >/dev/null \
  || fail "rotated claim does not verify"
grep -q "$after" "$conf" || fail "rotation did not redeploy the conf"
grep -q '(kind peer.rotation)' "$store/journal" || fail "rotation wrote no audit claim"

# ---- the root key is never in the store ---------------------------------
if grep -rq "$rootkey" "$store"; then fail "the root key is in the store"; fi

echo "peer_mint: ok (private claim, $(basename "$conf") deployed, heartbeat confirmed, rotation round-tripped, root key not stored)"
