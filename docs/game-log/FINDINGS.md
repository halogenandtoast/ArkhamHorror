# Game log: audit of the current system

Evidence for the design in `README.md`. Line numbers are as of the audit
(2026-10-05, `main` @ e75b5c06ba). Re-verify before relying on one.

## Coverage

Real log-line call sites in the engine (`send`, `sendI18n`, `sendEnemy`,
`sendEnemyOnly`, `sendReveal`, `sendRevelation`, `sendTarot`, `sendDrewCards`),
excluding `sendUI`/`sendAudio` (UI pokes, not log lines) and excluding the
definitions themselves: **39**.

Card implementation files:

| Directory | Files |
|---|---|
| `Arkham/Location/Cards` | 1303 |
| `Arkham/Asset/Assets` | 1177 |
| `Arkham/Treachery/Cards` | 761 |
| `Arkham/Enemy/Cards` | 700 |
| `Arkham/Event/Events` | 519 |
| `Arkham/Act/Cards` | 380 |
| `Arkham/Agenda/Cards` | 335 |
| `Arkham/Story/Cards` | 205 |
| `Arkham/Skill/Cards` | 157 |
| `Arkham/Investigator/Cards` | 97 |
| **Total** | **5634** |

Coverage is **0.7%**. Reproduce:

```sh
cd backend/arkham-api/library
grep -rn "^\s*\(send\|sendI18n\|sendEnemy\|sendEnemyOnly\|sendReveal\|sendRevelation\|sendTarot\|sendDrewCards\)\b" \
  --include="*.hs" . | grep -v "Classes/GameLogger.hs\|Helpers/GameLog.hs" | wc -l
```

The generic sites that *do* carry most of today's log are in
`Arkham/Game/Runner.hs` (card played, enemy drawn), `Arkham/Campaign/Runner.hs`
and `Arkham/Scenario/Runner.hs` (Record/Remember), and
`Arkham/Investigator/Runner.hs` (clue discovery, hand-size discard). Note
`Investigator/Runner.hs:1576-1577`: the real line is commented out and replaced
with `"discovered clue(s)"` — the count was lost and nobody could get it back.
That is the shape of the whole problem.

## The two string DSLs

`Arkham.Classes.GameLogger` defines `send :: HasGameLogger m => Text -> m ()`
and a `ToGameLoggerFormat` class with `format :: a -> Text`. Twelve instances
exist. They emit a brace mini-language:

- `Arkham/Card.hs:636` — `{card:"<name>":<CODE>:"<id>"}`, escaping with
  `T.replace "\"" "\\\""`
- `Arkham/Name.hs:89` — `{enemy:"<title>":"<eid>"}`
- plus `{investigator:…}`, `{location:…}` (two arities), `{token:…}`

Layered *on top* of that is the embedded-i18n DSL from `Arkham/I18n.hs:36`
(`ikey'` → `"$" <> key`, with `var=s:"…"` pairs appended by `withVar`). The two
compose only by `<>`, so a sentence that needs both a localized template and a
card chip cannot be written cleanly — which is why most call sites give up and
concatenate raw English.

## Transport

`Api/Handler/Arkham/Games/Shared.hs:750` — `handleMessageLog`:

- conses the text onto an `IORef` (already optimized away from O(n²), see the
  comment at :753)
- **and calls `broadcast` once per `ClientMessage`** (:757). The comment at
  :141-143 in the same file records that this is "hundreds of ~100 byte log
  lines" during scenario setup, each formerly paying a 256 KB zlib setup. The
  per-message zlib cost was fixed by enabling context takeover; the
  **per-message frame** was not.
- `toClientText` (:772) maps everything except `ClientText` to `Nothing`, so
  cards, draws, reveals, tarot and custom-card issues are **live-only** and
  absent from history.

`Arkham/Game.hs:699` — `data PublicGame gid = PublicGame gid Text [Text] Game`.
The third field is the entire log. Built by `toPublicGame`
(`Api/Arkham/Helpers.hs:77`) from `getGameLog` (:67), which has no `LIMIT`.

`frontend/src/arkham/components/GameLog.vue:14` —
`props.gameLog.slice(-10)`. The client renders ten entries.

## Persistence

`Entity/Arkham/LogEntry.hs` — `arkham_log_entries` is `body Text`,
`arkhamGameId`, `step Int`, `createdAt`. Written by
`Shared.hs:534`: `insertMany_ $ map (newLogEntry gameId arkhamGameStep now) updatedLog`.

`step` is load-bearing for undo: `Api/Handler/Arkham/Undo.hs:132, 180, 267, 385`
all delete/query entries by step. Any new shape must keep it.

`Api/Arkham/Export.hs:28` carries `agedLog :: [ArkhamLogEntry]` into exports,
and `Api/Handler/Arkham/Game/Debug.hs:272` rebuilds a `GameLog` from
`arkhamLogEntryBody`. Both need to handle the new payload.

## The narrator seam already exists

`Arkham/Game.hs:6795-6800`:

```haskell
Just msg -> do
  when (debugLevel == 1) $ ...
  for_ mLogger $ liftIO . ($ msg)
```

`runMessages :: … => Text -> Maybe (Message -> IO ()) -> m ()`
(`Arkham/Game.hs:6697`). This fires for **every** message popped, before it
runs. Its only current consumer is `collectFromRun` in `Shared.hs:483`, which
harvests achievements and phase entries.

Two gaps to close for narration: the hook is `IO` (no game access), and there is
no post-run hook (needed for before/after values).

## Frontend

`frontend/src/arkham/components/GameMessage.vue` — a `defineComponent` with a
`render()` that:

- calls `handleEmbeddedI18n`, then `.replace()`s a legacy
  `{token:"CustomToken …""}` shape written by an older build;
- `split`s on `/({[^}]+})/`;
- runs **seven** `regex.test(split)` / `split.match(regex)` pairs per fragment,
  each regex duplicated between the test and the match;
- builds nodes with `h('span', { 'data-image-id': … })`.

No memoization: this re-runs for every fragment on every render.

`frontend/src/arkham/views/Game.vue`:
- `:419` `gameLog = shallowRef<readonly string[]>(Object.freeze([]))`
- `:710-721` `updateGameLog` compares length + first + last, then
  `Object.freeze([...nextLog])` — a full copy of the whole history
- `:1368-1370` socket `GameMessage` → `Object.freeze([...gameLog.value, contents])`
  — another full copy, per line
- `:1013, 1161, 1205, 1337` four more `updateGameLog` call sites on fetch/refetch

`frontend/src/arkham/components/GameLog.vue`:
- `:14` slices the last 10
- `:34-40` `watch(truncatedGameLog, …, { deep: true })` then `scrollIntoView(false)`
  followed by `el.scrollTop = el.scrollTop + 100` — a magic nudge
- `:68` fixed `height: calc(100vh - 60px)`

`frontend/src/locales/en/log.json` — **three keys**. Everything else in the log
is English hardcoded in Haskell.

## Baselines still to measure (Phase 0)

Not yet measured. Record the numbers here when taken.

- [ ] Websocket frame count and total bytes during a scenario setup
      (Chrome DevTools, `list_network_requests` on the game socket).
- [ ] `PublicGame` payload size vs. its log field, mid-campaign.
- [ ] Row count in `arkham_log_entries` for a completed campaign.
- [ ] `GameMessage.vue` render cost — scripting time in a performance trace
      while a log-heavy setup streams in.
- [ ] Baseline narrative: `arkham-replay --trace <export.json> 2>&1 | grep '^client>'`
      for a Core Set scenario, saved as the before-picture.

Blocked on a game export; none is checked in. Get one from
`/api/v1/arkham/games/:id/export` against a local dev game.
