# arkham.build OAuth

Links a user's account here to their [arkham.build](https://arkham.build) account,
so we can read the decks they keep there — account decks, and the ArkhamDB decks
they have linked to it — without them pasting a deck URL for each one.

Upstream docs: [oauth-integration.md](https://github.com/arkham-build/arkham.build/blob/main/docs/oauth-integration.md),
[oauth-user-api.json](https://github.com/arkham-build/arkham.build/blob/main/docs/openapi/oauth-user-api.json).

## What is registered where

We are a **confidential client**: the client secret never leaves the server, and
neither token is ever served to the browser. An arkham.build administrator issues
a UUID `client_id` and a one-time `client_secret`, and registers our redirect
URIs. The server compares a redirect URI as an **exact string**, so every
environment's URI has to be registered separately:

| Environment | Redirect URI to register |
| --- | --- |
| local dev | `http://localhost:8080/api/auth/arkham-build/callback` |
| production | `https://arkhamhorror.app/api/auth/arkham-build/callback` |

Both go to the API, not the SPA — the code exchange needs the client secret. They
sit at `/api/auth/...` rather than under `/api/v1` because that is the path the
administrator registered, and the comparison is exact. They work because the SPA
and the API are the same origin in both places:
Vite proxies `^/api` to `127.0.0.1:3002` in dev, and nginx proxies `^~ /api` to
`localhost:3002` in production.

## Configuration

`config/settings.yml`, all overridable by environment variable:

| Setting | Env var | Default |
| --- | --- | --- |
| `arkham-build-api-base` | `ARKHAM_BUILD_API_BASE` | `https://api.arkham.build` |
| `arkham-build-client-id` | `ARKHAM_BUILD_CLIENT_ID` | — |
| `arkham-build-client-secret` | `ARKHAM_BUILD_CLIENT_SECRET` | — |
| `arkham-build-redirect-uri` | `ARKHAM_BUILD_REDIRECT_URI` | the table above, by build type |

With no client id or secret the integration is simply off: `GET
/api/v1/arkham-build/connection` answers `configured: false` and the settings
panel says so instead of offering a button that cannot work.

## Testing locally

```bash
cd backend
ARKHAM_BUILD_CLIENT_ID=019c... ARKHAM_BUILD_CLIENT_SECRET=ab_cs_... make api.watch
```

Then `npm run dev` in `frontend/`, open <http://localhost:8080/settings>, and use
the **Connections** tab. Reaching the consent screen proves the client id and
redirect URI match what was registered; coming back to a filled-in panel proves
the code exchange and the client secret.

## Scopes

arkham.build defines exactly four. `profile:read` is required of every
authorization request; the write and delete scopes imply the ones below them, and
the server expands and de-duplicates whatever is asked for.

| Scope | Requested | Why |
| --- | --- | --- |
| `profile:read` | yes | Required, and names the account in the settings panel |
| `decks:read` | yes | The decks to import, including the ArkhamDB decks linked there |
| `decks:write` | yes | `POST .../upgrade` creates a child deck linked to its parent — the campaign upgrade model, so upgrades, exiles and earned cards can be recorded back |
| `decks:delete` | **no** | Nothing here has a reason to destroy a player's decks, and it is the one scope that could |

`AB.requestedScopes` is the list. Growing it does not widen an existing grant:
each token carries only what was approved at its own authorization. The status
endpoint therefore reports `missingScopes`, and the panel asks the user to
reconnect while the connection keeps working for everything already granted.

## Endpoints

| Route | Auth | Does |
| --- | --- | --- |
| `GET /api/v1/arkham-build/connection` | JWT | Whether the server is configured, and this user's connection |
| `POST /api/v1/arkham-build/authorize` | JWT | Starts an authorization, returns the URL to send the browser to |
| `GET /api/auth/arkham-build/callback` | none | arkham.build's redirect target; redirects back to `/#<returnTo>?arkhamBuild=<outcome>` |
| `POST /api/v1/arkham-build/verify` | JWT | Calls `/v2/user/me` with the stored token, refreshing it first if needed |
| `DELETE /api/v1/arkham-build/connection` | JWT | Revokes the grant upstream and drops the row |

The frontend router uses **hash history**, so the outcome has to ride inside the
fragment (`/#/settings?arkhamBuild=connected`) and is readable only through
vue-router — `window.location.search` is empty on that page. A redirect to
`/settings?...` would instead load the SPA at an empty hash and land on Home.

The callback is necessarily unauthenticated: it arrives as a browser redirect
carrying no `Authorization` header. What it is allowed to do comes entirely from
the `arkham_build_auth_requests` row that the OAuth `state` names — written by
the authenticated `authorize` call, consumed on first use. That row is both the
CSRF check the spec asks for and the only thing that says whose connection is
being completed.

## Tokens

An access token lasts an hour; a refresh token lasts 90 days, and 180 after its
first rotation. Each refresh token works **exactly once** and returns its
replacement, so `arkhamBuildAccessToken` holds the connection row `FOR UPDATE`
across the exchange: two concurrent refreshes would otherwise each spend a token
and leave a connection that can no longer be refreshed at all.

Both tokens are stored in the clear, because they have to be usable — a digest
cannot be sent as a bearer token. What limits the damage is scope: we request
`profile:read decks:read`, so a leaked token reads decks and nothing else. It
cannot write or delete a deck, and it cannot touch anything on this site.

## Files

- `backend/arkham-api/library/ArkhamBuild/OAuth.hs` — the protocol: URLs, token
  exchange, refresh, revoke, `/v2/user/me`. No database, no `Handler`.
- `backend/arkham-api/library/Api/Handler/Arkham/BuildConnection.hs` — the policy:
  who owns a connection, when to refresh it, where to send the browser.
- `backend/arkham-api/library/Entity/Arkham/BuildConnection.hs`,
  `.../BuildAuthRequest.hs` — the two tables.
- `migrations/deploy/arkham_build_connections.sql` — both tables.
- `frontend/src/components/ArkhamBuildConnection.vue` — the Connections panel in
  Settings, and `components/icons/ArkhamBuildLogo.vue` for its mark.

## Not done yet

Reading decks. The scopes and the token plumbing are in place, so the next step is
`GET /v2/user/decks/manifest` plus `POST /v2/user/decks/batch` behind an endpoint
that offers the user's arkham.build decks as import candidates, reusing
`normalizeArkhamBuildDeckCodes` from `Api.Handler.Arkham.Decks` to translate
custom-card ids.
