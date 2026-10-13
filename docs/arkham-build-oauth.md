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
| `GET /api/v1/arkham-build/decks` | JWT | The user's decks, as chain tips, ready to import |
| `DELETE /api/v1/arkham-build/connection` | JWT | Revokes the grant upstream and drops the row |

The UI for both deck-facing pieces is behind `isDevBuild()` for now — the
Connections tab in Settings and the arkham.build source tab on the new-deck page.
The routes themselves are not gated; they fail closed without credentials.

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

An access token lasts an hour; a refresh token lasts 180 days, both as issued and
after each rotation — `AB.refreshLifetime`. Each refresh token works **exactly
once** and returns its replacement, so `arkhamBuildAccessToken` holds the
connection row `FOR UPDATE` across the exchange: two concurrent refreshes would
otherwise each spend a token and leave a connection that can no longer be
refreshed at all.

Both tokens are stored in the clear, because they have to be usable — a digest
cannot be sent as a bearer token. What limits the damage is scope: a leaked token
can read and write the decks on that arkham.build account, and nothing else. It
cannot delete one, and it cannot touch anything on this site.

arkham.build also offers **optional PKCE** (S256 only). We do not use it. PKCE
exists for a public client that cannot keep a secret, and the upstream docs are
explicit that it does not replace confidential-client authentication. Adopting it
would mean storing a verifier per authorization request for no gain.

Every `/v2/*` route shares one rate limit: **100 requests per 60 seconds per
client IP**, which blocks for a further 60 seconds once tripped. In production
that is one IP for every player at once, so it is a per-deployment budget, not a
per-player one — `AB.rateLimited` picks the 429 out of a `Failure`. It is the
reason a deck list is one manifest plus batches of 250 rather than a read per
deck.

## What we call upstream

The whole user API is eleven routes. `ArkhamBuild.OAuth` can now call ten of
them; which of those have a caller is a separate question.

| Upstream route | Scope | `AB.` function | Called from |
| --- | --- | --- | --- |
| `GET /v2/oauth/authorize` | — | `authorizeUrl` | `postApiV1ArkhamBuildAuthorizeR` |
| `POST /v2/oauth/token` | — | `exchangeCode`, `refreshTokens` | the callback, and `arkhamBuildAccessToken` |
| `POST /v2/oauth/revoke` | — | `revokeRefreshToken` | `deleteApiV1ArkhamBuildConnectionR`, and account deletion |
| `GET /v2/user/me` | `profile:read` | `fetchProfile` | the connection and verify endpoints |
| `GET /v2/user/decks/manifest` | `decks:read` | `fetchDeckManifest` | `getApiV1ArkhamBuildDecksR` |
| `POST /v2/user/decks/batch` | `decks:read` | `fetchDeckBatch` | `getApiV1ArkhamBuildDecksR` |
| `GET /v2/user/decks/{source}/{id}` | `decks:read` | `fetchDeck` | nothing yet — see *Syncing a deck* |
| `POST /v2/user/decks/{source}/{id}/upgrade` | `decks:write` | `upgradeDeck` | nothing yet — see *Writing an upgrade back* |
| `PUT /v2/user/decks/{source}/{id}` | `decks:write` | `replaceDeck` | nothing yet |
| `POST /v2/user/decks/{source}` | `decks:write` | `createDeck` | nothing yet |
| `DELETE /v2/user/decks/{source}/{id}` | `decks:delete` | — | not written: the scope is not requested, so the call could only ever 403 |

### Addressing a deck

A deck is a provider and an id, and an id means nothing to the other provider:
`account` ids are strings, `arkhamdb` ids are positive integers. `DeckRef` keeps
the id as the JSON it arrived as so it goes back unchanged, and renders it into a
path segment only when a route needs one. An id that is neither shape fails
locally rather than becoming a request.

### Writing a deck

A write replaces **all** mutable content, while silently ignoring the
server-owned fields (`id`, `source`, `user_id`, `date_creation`, `date_update`,
`version`, `previous_deck`, `next_deck`) mixed in with it.

That matters because our `ArkhamDBDecklist` models nine of `OAuthDeck`'s
twenty-four fields. Sending one as a write would erase everything it does not
model — the player's `description_md`, their `tags`, their `exile_string`, the
`xp` figures. So `DeckWrite` is the deck object itself rather than a record:
`deckWrite` starts from a deck as read and the `with*` functions change one key,
leaving the rest exactly as arkham.build had it. `newDeckWrite` is for a deck
that did not come from there, and sets only the six fields the API requires.

`meta` deserves the same care in the other direction: it is a JSON document
inside a string, holding customizations, attachments, the extra deck and the
alternate front. `withMeta` replaces the whole string, so a caller must edit it,
not rebuild it.

## Where the rest would go

### Syncing a deck

`postApiV1ArkhamSyncDeckR` re-reads a deck from its `url`. An arkham.build
**account** deck has no public URL, so the button cannot work on anything
imported through OAuth — and an arkham.build share URL is a version-pinned
snapshot anyway (#5257). `fetchDeck` is the fix, but it needs the row to remember
the `DeckRef` it came from: `arkham_decks` stores `url Text Maybe` and nothing
else, so an OAuth-imported deck currently has no way back to its source. That is
a migration, not a handler change.

### Writing an upgrade back

`putApiV1ArkhamGameDecksR` is the campaign's upgrade window. Today the player
leaves, upgrades on arkham.build by hand, and pastes the result back; the handler
only mirrors what it was given into `arkham_decks`.

The engine already knows what the upgrade is worth: `campaignXpBreakdown`
(`XpBreakdown`/`XpEntry`) is the experience earned, `campaignDecks` is the deck
as the scenario left it, and `campaignStoryCards` the cards earned in play.
`POST .../upgrade` takes exactly that shape — the child's desired content
including its **total** `xp`, which may not be below what carries over from the
parent, and with the same investigator. So the realistic first write is to create
the child deck pre-loaded with the XP the campaign awarded and the exiles it
recorded, and let the player spend it on arkham.build rather than re-entering it.

Two upstream behaviours shape that handler. A parent that already has a child is
a 409, which makes the call non-idempotent in a place that already has a
resubmit problem (#5256) — the same guard that stops a double upgrade has to
cover the write. And for an ArkhamDB parent, a child can be created upstream and
then fail to be written: the API returns 409 with the new child's id, and the
child stays linked in ArkhamDB. There is no rollback, so that id has to be
recorded rather than discarded.

### Creating and replacing

`createDeck` would export a deck built here — a homebrew list, or one assembled
in the card builder — to the player's arkham.build account. `replaceDeck` fits
a deck we already own there and want to keep in step without starting a new
history chain. Neither has a user-facing reason yet.

## Files

- `backend/arkham-api/library/ArkhamBuild/OAuth.hs` — the protocol: every call,
  and the DTOs they carry. No database, no `Handler`.
- `backend/arkham-api/library/Api/Handler/Arkham/BuildConnection.hs` — the policy:
  who owns a connection, when to refresh it, where to send the browser.
- `backend/arkham-api/library/Entity/Arkham/BuildConnection.hs`,
  `.../BuildAuthRequest.hs` — the two tables.
- `migrations/deploy/arkham_build_connections.sql` — both tables.
- `frontend/src/components/ArkhamBuildConnection.vue` — the Connections panel in
  Settings, and `components/icons/ArkhamBuildLogo.vue` for its mark.
- `frontend/src/arkham/components/NewDeck.vue` — the **arkham.build** source tab
  on the new-deck page, which lists what `getApiV1ArkhamBuildDecksR` returns.

