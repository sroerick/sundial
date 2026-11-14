# Decisions

## Node connector (card 43a6c99d)

- S2 scheduler design 9f2fd78a is the nave: the record, the schedule
  grammar, the durable run-record store and the outbox ship first. The node
  connector is built on top of it, never beside it.
- P1, the doorbell: the webhook receiver reuses tuna's sender-key pattern,
  card 35b5f79c - ONE doorbell shape town-wide. A signed POST answers 200
  and lands an `event.webhook` claim in the journal carrying `(action none)`;
  ingress stays dumb and auditable and acts on nothing. An unsigned or
  mis-signed POST answers 401 and writes nothing at all.
- P2, the wireguard administrator: a static-mesh KEY AUTHORITY, not a
  coordination server (tailscale/nebula have no first-class OpenBSD support,
  which is fatal for a wyo-seated town). Peer configs are PRIVATE-scope
  claims under `private/peers/`, minted at a tier, audited and rotated by the
  sundial cron on schedule; the factory deploys the generated conf. The root
  signing key lives only in the daemon environment (`PP_NODE_ROOT_KEY`) and
  is never written into the store.
- The 10-node/roaming trigger: at ten nodes, or the first roaming laptop,
  the town rotates from a hand-kept static mesh to per-peer minted keys with
  scheduled audit and rotation.
- Tier law: a peer may be minted at or below its allowance
  (`host` < `relay` < `exit`). An above-tier mint is refused non-zero and
  writes nothing.
