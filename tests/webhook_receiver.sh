#!/bin/sh
# card 43a6c99d (P1): the doorbell. A signed POST answers 200 and lands one
# event.webhook claim in the journal; an unsigned POST answers 401 and writes
# nothing. Ingress acts on nothing - the claim is the whole product, and it
# carries (action none). ONE doorbell shape town-wide (35b5f79c).
set -eu

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)
if [ -x "$root/pp_node.exe" ]; then
  bin=${PP_NODE:-$root/pp_node.exe}
else
  bin=${PP_NODE:-$root/_build/default/pp_node.exe}
fi

[ -x "$bin" ] || { echo "webhook_receiver: no pp_node at $bin" >&2; exit 1; }

fail() { echo "webhook_receiver: $*" >&2; exit 1; }

store=$(mktemp -d "${TMPDIR:-/tmp}/pp-node-hook.XXXXXX")
trap 'rm -rf "$store"' EXIT

key=doorbell-sender-key-0123456789abcdef
body='{"action":"ping","src":"tuna"}'

# ---- unsigned: 401, nothing written -------------------------------------
if printf '%s' "$body" | "$bin" receive --store "$store" --key "$key" \
     >/dev/null 2>&1; then
  fail "unsigned POST was not refused"
fi
[ ! -e "$store/journal" ] || fail "unsigned POST wrote to the journal"

# ---- mis-signed: 401, nothing written -----------------------------------
if printf '%s' "$body" | "$bin" receive --store "$store" --key "$key" \
     --signature deadbeef >/dev/null 2>&1; then
  fail "mis-signed POST was not refused"
fi
[ ! -e "$store/journal" ] || fail "mis-signed POST wrote to the journal"

# ---- signed: 200, one event claim, action none --------------------------
sig=$(printf '%s' "$body" | "$bin" sign --key "$key")
out=$(printf '%s' "$body" | "$bin" receive --store "$store" --key "$key" \
        --signature "$sig")
case $out in
  *200*) : ;;
  *) fail "signed POST did not answer 200 (got '$out')" ;;
esac

[ -f "$store/journal" ] || fail "signed POST wrote no journal"
grep -q '(kind event.webhook)' "$store/journal" || fail "no event.webhook claim"
grep -q '(scope public)' "$store/journal" || fail "event claim is not public"
grep -q '(door 35b5f79c)' "$store/journal" || fail "claim does not name the door"

# Ingress acted on nothing: no action but (action none), no peer, no conf.
grep -q '(action none)' "$store/journal" || fail "event claim does not say action none"
[ -z "$(sed 's/(action none)//g' "$store/journal" | grep -o '(action [^)]*)' || true)" ] \
  || fail "ingress acted on the body"
[ ! -d "$store/private" ] || fail "receiver minted a peer"
[ ! -d "$store/wg" ] || fail "receiver deployed a conf"

# ONE doorbell shape town-wide: DECISIONS.md names 35b5f79c.
grep -q '35b5f79c' "$root/DECISIONS.md" || fail "DECISIONS.md does not name 35b5f79c"

echo "webhook_receiver: ok (signed 200 + event claim (action none), unsigned 401, nothing acted on)"
