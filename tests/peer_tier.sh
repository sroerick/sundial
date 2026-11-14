#!/bin/sh
# card 43a6c99d (P2): tier law. A peer may be minted at or below its
# allowance (host < relay < exit); an above-tier mint exits non-zero and
# writes nothing at all.
set -eu

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)
if [ -x "$root/pp_node.exe" ]; then
  bin=${PP_NODE:-$root/pp_node.exe}
else
  bin=${PP_NODE:-$root/_build/default/pp_node.exe}
fi

[ -x "$bin" ] || { echo "peer_tier: no pp_node at $bin" >&2; exit 1; }

store=$(mktemp -d "${TMPDIR:-/tmp}/pp-node-tier.XXXXXX")
trap 'rm -rf "$store"' EXIT
wg=$(mktemp -d "${TMPDIR:-/tmp}/pp-node-tier-wg.XXXXXX")
trap 'rm -rf "$store" "$wg"' EXIT

PP_NODE_ROOT_KEY=root-secret-0123456789abcdef
export PP_NODE_ROOT_KEY

# At the allowance: allowed.
"$bin" peer mint --store "$store" --name host1 --tier host --addr 10.90.0.2 \
  --epoch 1 --max-tier host --wg-dir "$wg" >/dev/null
[ -f "$store/private/peers/host1.claim" ] || {
  echo "peer_tier: at-tier mint wrote no claim" >&2; exit 1; }

# Above the allowance: refused, non-zero, nothing written.
before=$(find "$store" -type f | sort)
wg_before=$(ls -A "$wg" | sort)
if "$bin" peer mint --store "$store" --name relay1 --tier relay \
     --addr 10.90.0.3 --epoch 1 --max-tier host --wg-dir "$wg" >/dev/null 2>&1; then
  echo "peer_tier: above-tier mint (relay > host) exited 0" >&2; exit 1
fi
[ ! -f "$store/private/peers/relay1.claim" ] || {
  echo "peer_tier: above-tier mint wrote a claim" >&2; exit 1; }
after=$(find "$store" -type f | sort)
[ "$before" = "$after" ] || {
  echo "peer_tier: above-tier mint changed the store" >&2; exit 1; }
[ "$wg_before" = "$(ls -A "$wg" | sort)" ] || {
  echo "peer_tier: above-tier mint deployed a conf" >&2; exit 1; }

echo "peer_tier: ok (at-tier minted, above-tier refused with nothing written)"
