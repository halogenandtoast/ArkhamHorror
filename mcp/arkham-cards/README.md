# arkham-cards — an MCP server for writing custom cards

Two transports over one set of tools:

| | |
|---|---|
| `server.py` | stdio, single user, this machine. Registered in `/.mcp.json`. |
| `http_server.py` | remote, multi-user, authenticated per request. See **Running it remotely**. |

The tools live in `lib/tools.py` and are shared, so one cannot exist locally and
be missing remotely, or be scoped differently in the two. Standard library only:
no install step, nothing to keep up to date but the repo itself.

## The security model, in one paragraph

**The remote server holds no secret and decides no permissions.** It takes the
caller's `Authorization` header, forwards it to arkham-api, and relays the answer.
It cannot mint a token, cannot name a user, and cannot widen a scope — so a bug in
it cannot become access to somebody's account. The scope declarations in
`lib/tools.py` only decide which tools a caller is *offered*; every call is checked
again by arkham-api, which is the only thing that should be checking. The local
stdio server does mint its own token, which is fine on the machine that holds the
database anyway and is exactly what the remote one must never do.

## Why a server rather than a skill

A custom card's behaviour is JSON in the card def's `meta`, decoded at the moment
it runs. A value in the wrong shape is never an error — it fails to parse and
whatever contained it keeps its default, so an ability whose `type` will not
decode is dropped and the card silently has no ability at all.

Two consequences, and they are what the tools are for:

1. **The card has to be checked against the types before it is saved.** That is a
   mechanical check, and `validate_card` does it. Run against the 44 cards in this
   library it reports 5 errors and 2 warnings, all genuine: two cards
   (Leroy Stanton, Working Overtime) have abilities that have never worked,
   `"actions": null` — the same shape that had Thirst for Knowledge's action dead
   from the day it was made.
2. **The reference has to come from the engine, not from prose beside it.** The
   step and expression languages are not types — they are `KeyMap.lookup "name" o`
   calls — so nothing reifies them into the schema endpoint, and a hand-written
   description of them is the copy that goes stale. `extract_dsl.py` reads them
   off the Haskell instead.

## The tools

| Tool | Scope | For |
|---|---|---|
| `guide` | — | the process, and the DSL grammar generated from the engine |
| `schema_search` | — | find a type or constructor by name |
| `schema_type` | — | how that type is actually written in JSON |
| `rules_search` | — | the rulings, in this project's source priority |
| `official_cards` | — | the printed card that already says this |
| `validate_card` | — | every way this def will silently fail |
| `whoami` | — | who this credential is, and what it may do |
| `card_sets` | cards:read | the caller's sets — a card is always saved into one |
| `custom_card_examples` | cards:read | precedent: a fragment known to decode |
| `save_card` | cards:write | the parser's own verdict, and the card in the library |
| `delete_card` | cards:write | remove a card from the library |
| `create_set` | cards:write | a new set to save into |
| `rename_set` | cards:write | rename a set |
| `delete_set` | cards:write | delete a set AND every card in it |

A tool with no scope is the same answer for everyone. A caller is offered only the
tools its scopes cover, so a `cards:read` key never sees `save_card` — but that is
presentation: arkham-api checks the scope again on every call.

There is also a `write-card` prompt (`/arkham-cards:write-card` in Claude Code)
that runs the whole process from a card's printed text.

## Regenerating the grammar

```sh
python3 mcp/arkham-cards/extract_dsl.py
```

Re-run after touching `Arkham/Custom/Steps.hs`, `Expr.hs`, `Ability.hs`,
`Card/CardDef.hs`, or any entity's `Attrs` record. It writes `dsl.json`, which
holds:

- every step, its keys, and **where those keys go** — a step the dispatch hands to
  a handler keeps its keys inside its own value, a step handled inline reads the
  step object. This is the distinction most worth having written down: putting a
  key in the wrong place means it is simply never read.
- the expression operators and every closed vocabulary (query kinds, card
  properties, `apply` transforms, …)
- the `_`-prefixed meta keys the engine reads, and which hold steps
- each entity's serialized field names, which are the `$bindings` a card gets for
  free and are nowhere in the schema
- which decoders are hand-written, and what each insists on — without this, every
  field a decoder defaults reads as missing
- which decoders also accept a bare array, so `"actions": []` is not reported as
  malformed

`extract_dsl.py` fails loudly if it cannot find `runSteps`, so a rename shows up
as a broken generator rather than a silently empty reference.

## Running it remotely

The remote server is **not a separate deployment**. It runs as a third process in
the existing `arkham-web` pod, behind the nginx that already lives there:

```
DO LoadBalancer :443  ──TLS──▶  pod :3000  nginx
                                   ├── /api  ──▶ localhost:3002  arkham-api
                                   ├── /mcp  ──▶ localhost:8420  http_server.py
                                   └── /     ──▶ the built frontend
```

That shape follows from the cluster: the image is already multi-process
(`start.sh` backgrounds `arkham-api`, then runs nginx in the foreground), and there
is **no Ingress controller** — nginx inside each pod routes by hostname. So adding
`/mcp` costs one `location` block and one line in `start.sh`, and **no new
Terraform resources**: no second LoadBalancer, no second certificate, no Service,
no service discovery. `ARKHAM_API=http://localhost:3002` is the same pod.

A separate Deployment would need either a second DO LoadBalancer (money, another
cert) or an nginx location proxying to an MCP Service — which reintroduces the
coupling at the routing layer *and* adds a hop and a second image to build.

### What ships, and how

`make v2-deploy-committed` pipes `git archive` into buildx, so **only committed
files reach the image**. That is why this lives in `mcp/` and not under
`.claude/`, and why `references/` and `data/` are tracked: an untracked file is
simply absent from production. `.claude/references` and `.claude/data` are
symlinks here so the paths in CLAUDE.md and the slash commands still resolve.

`dsl.json` is **generated during the build** by the `mcp` stage in the Dockerfile,
from the Haskell the DSL is implemented in. Never commit it: a checked-in
generated file is the one that goes stale, and the failure is silent.

### Deploying

Nothing new. The existing pipeline carries it:

```sh
make v2-deploy-committed        # build + push + rollout restart
```

Rollback is unchanged too — repoint `:latest` at the immutable per-commit tag and
restart, exactly as for the app.

### The credential, and what this process must not hold

Callers authenticate with a **personal API key** (Settings → API keys in the web
app — **admin accounts only** while this settles; the routes are gated in
`Foundation.isAuthorized` and the tab is hidden to match), sent as `Authorization: Bearer ak_...`. Scoped (`cards:read`,
`cards:write`), revocable on its own, and it records when it was last used — none
of which the account token is, which is why pasting *that* into a client is the
wrong answer.

`start.sh` launches the server with **`env -u JWT_SECRET`**. The app container
holds that secret because it signs login tokens, and any process holding it can
mint a token for *any* user id — precisely what a multi-tenant server must not be
able to do. `http_server.py` never reads it; unsetting it means a future change
that tried would fail instead of quietly impersonating somebody. The server says
which state it is in on its first line of output.

### Connecting

```sh
claude mcp add --transport http arkham-cards https://arkhamhorror.app/mcp \
  --header "Authorization: Bearer ak_..."
```

What a key may do and nothing more: read the library, create/update/delete cards,
create/rename/delete sets. Publishing to the marketplace, subscribing and set
import stay session-only, because they reach past the caller's own account.

### Limits

| | |
|---|---|
| writes | 300/hour, counted on the key row in Postgres — correct across replicas, and it holds even for someone calling the API directly |
| requests | 240/min per credential, in-process. **Per pod**, so the real ceiling is `replicas × 240` |

The write cap is the one that protects the database and the asset bucket, which is
why it lives there rather than here.

### Costs of the in-pod choice

- The MCP server restarts whenever the app does.
- ~50 MB of Python in the image, and a little more baseline RSS per pod. The HPA
  targets memory utilisation against a 1Gi request, so expect marginally earlier
  scale-out.
- `http_server.py` is `ThreadingHTTPServer`: a thread per request, blocking I/O,
  every request a short proxied call. Right for this load behind nginx; not an
  async server, so that is the thing to change if it ever fronts something heavy.

## Configuration

| Variable | Default | |
|---|---|---|
| `ARKHAM_API` | `http://localhost:3002` | needs `make api.watch` in `backend/` |
| `ARKHAM_REFERENCE_DIR` | the `mcp/` directory above this one | where `references/` and `data/` live; set to `/opt/arkham/mcp` in the image |
| `MCP_HOST` / `MCP_PORT` / `MCP_PATH` | `127.0.0.1` / `8420` / `/mcp` | remote only |
| `MCP_PUBLIC_URL` | derived | how the server describes itself in its 401 challenge; Terraform sets it from `var.domain` |
| `MCP_ALLOWED_ORIGINS` | none | an `Origin` header is refused unless listed |
| `MCP_REQUESTS_PER_MINUTE` | `240` | per credential |
| `ARKHAM_USER_ID` | `1` | **stdio only** — whose library |
| `JWT_SECRET` | read from `config/settings.yml` | **stdio only**; never set it on the remote server |

The schema is cached in `.schema-cache.json` for 15 minutes; when it expires, a
fetch that fails falls back to the stale copy and says so in `schemaSource`. So
once the cache has been warmed **at least once** with the API up, `schema_*`,
`validate_card`, `guide`, `rules_search` and `official_cards` all keep working
with the API down. `card_sets`, `save_card` and `custom_card_examples` need it
running, since they read and write the real library.

With no cache at all and no API, the schema tools fail with a message saying to
start it — they do not fall back to anything, because there is nothing to fall
back to. Warm it with:

```sh
python3 -c "import sys; sys.path.insert(0, 'mcp/arkham-cards'); from lib import schema; schema.load(force=True)"
```

## Layout

```
server.py         stdio transport: one user, this machine
http_server.py    remote transport: Streamable HTTP, stateless, per-request auth
extract_dsl.py    reads the DSL's grammar off the Haskell -> dsl.json
lib/tools.py      the tools and their scopes, shared by both transports
lib/schema.py     the schema endpoint, plus the JSON encoding each type uses
lib/validate.py   values against types, and the DSL against its grammar
lib/library.py    a caller's cards, through the API that owns them
lib/refs.py       the rules trees and the printed cards
lib/guide.py      the prose pages, and the generated grammar pages
guide/*.md        process, pitfalls, bindings
```

Deployment is not a directory here: it is three edits to files that already exist
— a build stage in `Dockerfile`, a line in `start.sh`, a `location` in
`prod.nginxconf` — plus one env var in `terraform/app.tf`.

`lib/library.py`'s cache is per-`Library`, never module-level. A cache keyed by
`"cards"` on a multi-tenant server hands one user's library to the next caller,
which is the worst bug this code could have; the interleaved two-user test in
**What this cannot do** is there to keep it fixed.

`lib/schema.py`'s encoding rules mirror `frontend/src/arkham/schema.ts`, which is
the authority — the card builder and an agent write into the same field, so the
two must agree.

## What this cannot do

Validation proves a card **decodes**. Nothing here plays it, so nothing here
proves it does what its text says — least of all anything about timing. Before
trusting a clause that turns on a window, an "instead", a cancel or a defeat, read
`mcp/references/engine-gotchas/`.

The remote server offers no OAuth flow, so connecting means pasting a key rather
than clicking through a consent screen. The key table and the scope check are
where an authorization server's access tokens would land, so that is an addition
rather than a rewrite when it is wanted.

`http_server.py` is `ThreadingHTTPServer` — a thread per request, blocking I/O,
and every request is a short proxied call. That is the right shape for this load
and behind nginx, but it is not an async server: if this ever fronts something
heavy, that is the thing to change.
