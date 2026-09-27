---
name: gameupdate-redis-path-vs-log-lines
description: GameMessage is broadcast in-process but GameUpdate round-trips through Redis pub/sub, so a dead subscriber freezes the board while log lines keep scrolling
metadata:
  node_type: memory
  type: project
---

The two kinds of websocket traffic take **different routes out of the server**, and
only one of them touches Redis:

- `GameMessage` / `GameUI` / `GameCard` / `GameError` / `GameTarot` … — emitted by
  `handleMessageLog` *during* `runMessages` straight into the in-process room
  (`broadcastToRoom`). Never leaves the pod.
- `GameUpdate` / `GameAchievement` / `SharedStateUpdate` / `EventChanged` — go through
  `publishToRoom` → Redis `PUBLISH` → a subscriber thread → `broadcastToRoom`.

So **"log lines keep arriving but the board is frozen" is the signature of a broken
Redis subscription**, not of a stuck engine. Reloading the page shows the action *was*
applied, and no `GameError` is emitted, because the publish itself succeeded.

Three separate bugs lived here, all fixed together:

1. **Subscription per connection, fanned out to the whole room.** Each websocket ran its
   own `runRedis … pubSub (subscribe [gameChannel])` whose callback broadcast to *every*
   subscriber on the room. With N sockets for one game on one pod, each publish was
   delivered N times to each of them (N² sends), and each open tab cost a Redis
   connection. Now the *room* owns exactly one subscription, created in `joinRoomIn` and
   torn down in `releaseRoomIfEmpty`. Verified with `redis-cli pubsub numsub`: 4 sockets
   → 1 subscriber, 1 delivery each.

   This also explains a confusing symptom: a *second* browser could "rescue" the first.
   Its healthy subscriber broadcast to the shared room, so the stuck tab received updates
   it could no longer fetch for itself — but only when both landed on the same pod.

2. **`pubSubForever` was forked bare.** hedis documents that it throws on connection loss
   and must be re-called to resubscribe everything in the controller. One Redis blip would
   have ended cross-pod delivery for the life of the pod. It is now supervised with capped
   backoff (`pubSubSupervisor`).

3. **A half-open subscriber socket is invisible.** An idle TCP connection dropped by a
   proxy leaves hedis blocked in `recv` forever — still "connected", delivering nothing,
   throwing nothing, so neither a retry loop nor `withPingThread` on the *websocket* helps.
   This is what stranded a game after ~30 idle minutes. Fixed with a heartbeat on
   `arkham:pubsub:health` (an initial controller subscription, so it survives every
   reconnect): each pod publishes over the normal pool and every pod is subscribed, so the
   beat travels the same route a `GameUpdate` does. Nothing received for
   `pubSubStaleSeconds` → tear down and reconnect. The beat doubles as keepalive, so
   normally the idle drop never happens.

Two invariants to preserve:

- **Join atomically.** `releaseRoomIfEmpty` reads "in the map with zero subscribers" as
  "nobody is here". Looking a room up and registering on it must happen in one turn of the
  rooms `MVar`, or a departing connection can release the room out from under an arriving
  one, which then silently receives nothing forever. This is why there is no
  create-without-joining helper — `getRoom`/`getRoomIn` were deleted rather than left as a
  footgun.
- **Don't swallow publish failures.** `publishToRoom` used to `void` away `runRedis`'s
  `Left Reply`. Use `publishOrWarn`.

Local dev has no Redis by default (`REDIS_CONN` empty → `WebSocketBroker`), so none of
this path is exercised. Use `make api.watch.redis`. See also
[[project_websocket_idle_warp_timeout]], which is the same class of bug one layer up (the
*websocket* being idled out rather than the Redis subscriber).
