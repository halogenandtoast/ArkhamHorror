# Arkham Horror Third Edition (3ed)

A second game — the Arkham Horror *board* game, third edition — shares this
repo's backend, accounts and websocket rooms, but has its own engine and its
own frontend, served from `3ed.arkhamhorror.app`.

## Pieces

| Where | What |
|---|---|
| `backend/ah3e/` | The engine: a standalone Haskell package (`AH3e.*`), pure `Game -> Either EngineError Game`. `dev/` holds a single-player dev viewer (`stack exec ah3e-dev`, port 3100). |
| `backend/arkham-api/library/ThirdEdition/` | Tables (lobby + game) and where they're stored. |
| `backend/arkham-api/library/Api/Handler/ThirdEdition.hs` | The `/api/v1/3ed/...` endpoints and the table websocket. |
| `frontend-3ed/` | The Vue app. `npm run dev` serves it on :8081, proxying `/api` to :3002. |
| `frontend-3ed/public/img/ah3e/` | Art (gitignored, on the CDN under `img/ah3e/`). `make fetch-images-3ed` downloads it. |

## Accounts

There is no separate registration. The main frontend keeps its sign-in token
in an `arkham-token` cookie (`Domain=.arkhamhorror.app` in production, via the
`AUTH_COOKIE_DOMAIN` build arg; host-only locally, so every port shares it),
falling back to localStorage. `frontend-3ed` reads the same cookie and sends
the usual `Authorization: Token …` header. Signed-out players are sent to the
main site to sign in.

## Tables

A table is a lobby of 1–6 seats; seat *n* is the engine's `PlayerId n`. The
host starts it once every seat is taken (one user may hold several seats).
Only a seat's holder can answer its questions; any seated player can undo, and
use debug actions on tables created with debug on.

Tables are **not** in Postgres. With Redis configured (production) they live
there — `ah3e:table:<id>`, a capped undo history `ah3e:history:<id>`, and the
index sets `ah3e:open` and `ah3e:user:<id>` — and expire after two weeks idle,
so they survive refreshes, pod restarts and deploys. Writes are optimistic
(`WATCH`/`MULTI`), so concurrent moves on different pods retry rather than
clobber each other. Without Redis (default local dev) they live in process
memory and vanish when the API restarts.

Every change is published to the table's room (Redis channel
`ah3e-<table id>`), whose websocket is a read-only feed; moves go over REST.
Clients keep whichever copy of the table has the higher `version`. None of
this touches the Arkham rooms, their Redis registry, or the heartbeat.

## Deploy

- The Docker image builds `frontend-3ed` in its own stage into
  `/opt/arkham/src/frontend-3ed/dist`.
- `prod.nginxconf` serves it for `server_name 3ed.arkhamhorror.app` (the exact
  name wins over the `.arkhamhorror.app` wildcard) and on port 3001 for
  docker-compose, where there's no hostname to route on.
- `terraform/ingress.tf` adds `3ed.<domain>` to the Let's Encrypt certificate.
  DNS: a `CNAME 3ed -> arkhamhorror.app.` in the DigitalOcean zone, created
  before `terraform apply`.

## Images

`frontend-3ed/image-manifest.json` is generated alongside the main one by
`make generate-manifest`; `make sync-images` / `sync-and-manifest` upload
`frontend-3ed/public/img/ah3e` to `s3://arkham-horror-assets/img/ah3e`;
`scripts/fetch-assets.sh` puts `img/ah3e/` keys back under `frontend-3ed/public`
(targets `en`, `en+<lang>`, `all` and `3ed`).

## Removing it

Delete `backend/ah3e`, `frontend-3ed`, `backend/arkham-api/library/ThirdEdition`,
`Api/Handler/ThirdEdition.hs` and this file, then undo the small hooks:

- `backend/stack.yaml` (`- ah3e`), `backend/arkham-api/package.yaml` (`- ah3e`)
- `config/routes` (the `/3ed` block), `Foundation.hs` (`appThirdEditionRooms`,
  `appThirdEditionStore`), `Application.hs` (their setup and the handler import)
- `Dockerfile` (the `frontend-3ed` stage, its `COPY --from`, the `ah3e`
  `package.yaml` copy, port 3001), `prod.nginxconf` (the second server block),
  `docker-compose.yml` (port 3001, the two `frontend-3ed` mounts)
- `dev.up` (the `WEB3ED` server), `Makefile` (`fetch-images-3ed`, the 3ed line
  in `sync-images`, and `ah3e/library ah3e/dev` in `WATCH_SRC` so the watch loops
  rebuild on engine edits), `scripts/` (`generate-manifest.cjs` app list,
  `sync-and-manifest.sh`, `check-manifest.sh`, `fetch-assets.sh`)
- `terraform/ingress.tf` (the `3ed.` domain), `.gitignore`
  (`frontend-3ed/public/img/`)

The shared sign-in cookie (`frontend/src/authToken.ts`) is worth keeping for
any future subdomain.
