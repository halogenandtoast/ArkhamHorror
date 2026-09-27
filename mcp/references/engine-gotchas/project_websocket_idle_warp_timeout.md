---
name: websocket-idle-warp-timeout
description: Warp's 30s idle timeout tore down quiet game websockets, so every open tab silently reconnected on a 32s cycle; the access log logged each one as a bogus 500 at close time
metadata:
  node_type: memory
  type: project
---

Warp treats a websocket as a **raw response** (`webSockets` → `sendRawResponseNoConduit` →
`ResponseRaw`), and `warp/Network/Wai/Handler/Warp/Response.hs` only calls `T.tickle th` from
the raw `recv`/`send` callbacks. Nothing else keeps the timeout handle alive. With
`warpSettings` built on `defaultSettings` (`settingsTimeout = 30`), **a game where nobody is
taking a turn is killed after 30 seconds of socket silence.** `useWebSocket` in
`views/Game.vue` has `autoReconnect: true` and no `heartbeat`, so the client immediately
reconnects: an endless ~32s churn cycle (30s idle + ~1s reconnect delay + handshake) per open
tab, each cycle re-running `subscribeToRoom` / `incrRoomMember` and the Redis room bookkeeping.

Two symptoms that mislead:

- **The bogus `500`.** `mkRequestLogger` reads `responseStatus`, and a `ResponseRaw` has no
  real HTTP status, so every game socket is logged as
  `GET /api/v1/arkham/games/<id>?token=… 500`. Nothing failed.
- **The line appears at close, not at open.** wai-extra logs after `sendResponse` returns,
  which for a hijacked connection is when the socket *closes*. So log timestamps are
  disconnect times, and a steady 30s spacing between them is the tell for this bug — not a
  heartbeat, and not a polling client. (`?token=` is the websocket URL, built by
  `buildWebsocketUrl` in `api.ts`; the REST `fetchGame` uses `?_=<timestamp>` instead.)

Fixed in `Api/Handler/Arkham/Games/Shared.hs` with `withKeepAlive`, wrapping both `gameStream`
and `streamRoom` in `websockets`' `withPingThread conn 15`. A ping is a real `connSendAll`, so
it tickles Warp's handle, and it keeps intermediate proxies (the Vite dev proxy, nginx,
CloudFront) from idling the connection out too. Raising `settingsTimeout` instead would have
weakened the idle timeout for all ordinary HTTP and done nothing about those proxies.

`Application.hs` also gained `skipWebSocketLogging`, which routes requests carrying
`Upgrade: websocket` around the access logger entirely — the log line was never meaningful for
a hijacked connection.

Unrelated to the `develMain` / unix-dependency removal (ff3eb2d94c) it was first noticed
after: `make api.watch` ran `stack exec arkham-api` → `appMain` → `warpSettings` both before
and after that commit, byte-identical. The churn predates it.
