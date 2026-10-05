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

## Baselines — measured 2026-10-05

Source: game `8572c1df-440e-4f55-a136-90c66d962cd5`, "The Innsmouth Conspiracy",
2 investigators (Amanda Sharpe, Dexter Drake), **2,885 steps**. Local dev API.

Artefacts kept in the session scratchpad (regenerate with the commands below;
they are not checked in): `export266.json`, `publicgame.json`,
`trace_all200.txt`.

### Coverage, measured on real play

Replaying 2 steps of this game with `--replay-all --undo 200 --trace`:

| | |
|---|---|
| engine messages processed | **3,111** |
| client messages emitted | **6** |
| of those, actual log lines (`ClientText`) | **1** |
| **log lines per message processed** | **0.03%** |

The other 5 were `ClientDrewCards` (dropped from history by `toClientText`)
and one `ClientError`.

A single step's trace is the whole problem in 14 lines:

```
> Record (TheInnsmouthConspiracyKey TheInvestigatorsReachedFalconPointBeforeSunrise)
client> text Record "the investigators reached falcon point before sunrise"
> ReportXp [AllGainXp {details = XpDetail {source = XpFromVictoryDisplay, sourceName = "Intersection", amount = 1}},
           AllGainXp {details = XpDetail {source = XpFromVictoryDisplay, sourceName = "Fork in the Road", amount = 1}},
           AllGainXp {details = XpDetail {source = XpFromVictoryDisplay, sourceName = "Desolate Road", amount = 1}},
           AllGainXp {details = XpDetail {source = XpFromVictoryDisplay, sourceName = "Cliffside Road", amount = 1}}]
> GainXP "07002" ScenarioSource 4
> GainXP "07004" ScenarioSource 4
> EndOfGame Nothing
> EndOfScenario Nothing
...
```

A player finishing a scenario is told one thing: a campaign-log record. Not
that they earned 4 XP, not which victory-display cards paid for it — although
`ReportXp` carries `XpDetail {source, sourceName, amount}`, fully structured,
right there in the message. **The information already exists and the log throws
it away.** That is the argument for deriving, in one screenshot.

### What the 481 stored entries actually say

The whole campaign produced **481 log entries** — about one per six steps. By
content:

| Share | Count | What it says |
|---|---|---|
| 60.3% | 290 | `X draws [token] chaos token` |
| 22.2% | 107 | `X played Y` |
| 15.4% | 74 | `X discovered N clue` |
| 1.5% | 7 | campaign-log `Record` |
| 0.6% | 3 | everything else |

**98% of the log is three sentences.** In practice the log is a chaos-token
ticker. Damage, horror, enemy attacks, spawns, evades, fights, encounter draws,
act and agenda advancement, XP, skill-test results, resource gain and movement
produce *nothing at all*.

### The clue count is a confirmed regression

All 74 clue entries in stored history use the **old** form with the number
(`discovered 1 clue`). Zero use the current `discovered clue(s)`
(`Investigator/Runner.hs:1577`). The count used to be in the log, and the
string DSL is why it is not any more.

### Payload waste, measured

`GET /api/v1/arkham/games/:id` for this game:

| Field | Bytes | Share |
|---|---|---|
| `cards` | 146,296 | 57.9% |
| **`log`** | **51,934** | **20.5%** |
| `mode` | 25,819 | 10.2% |
| `investigators` | 22,353 | 8.8% |
| `modifiers` | 3,940 | 1.6% |
| **whole payload** | **252,726** | |

The log is the **second-largest field in `PublicGame`** — bigger than every
investigator combined.

`GameLog.vue:14` renders the last **10** entries. So of the 51,934 bytes sent,
**2.08% is used and 50,959 bytes are discarded** — on every fetch, and there
are five `updateGameLog` call sites (`Game.vue:1013, 1161, 1205, 1337` plus the
socket append).

Entry length: min 41, median 88, mean 98, max 198 bytes.

### The renderer parses for refs that never occur

Across all 481 entries, only **three** ref kinds appear:

| Ref | Occurrences |
|---|---|
| `{investigator:…}` | 474 |
| `{token:…}` | 356 |
| `{card:…}` | 107 |
| `{enemy:…}` | **0** |
| `{location:…}` | **0** |

`GameMessage.vue` runs **seven** regex `test`+`match` pairs per fragment per
render. Three of them (`enemy`, and both `location` arities) never match in this
corpus, and they are evaluated for every fragment of every render regardless.

### Message traffic is 64% plumbing, and `Message` is not flat

Top constructors over the 3,111 processed messages:

| Count | Constructor | |
|---|---|---|
| 621 | `Do` | wrapper |
| 384 | `CheckWindows` | plumbing |
| 372 | `EndCheckWindow` | plumbing |
| 200 | `ClearUI` | plumbing |
| 200 | `Ask` | plumbing |
| 174 | `WindowAsk` | plumbing |
| 149 | `SkillTestMessage` | **wrapper — payload inside** |
| 114 | `MoveWithSkillTest` | wrapper |
| 89 | `ResolvedAbility` | |
| 88 | `SetActiveInvestigator` | plumbing |
| 83 | `After` | wrapper |
| 68 | `ResolveWindowInitiations` | plumbing |
| 55 | `TakenActions` / `FinishAction` | |
| 44 | `PhaseStep` | **structure — log this** |
| 32 | `DrawEnded` | |

Two consequences for the narrator, both learned here rather than assumed:

1. **~64% of messages are plumbing** and must never produce a line. The
   narrator's default case has to be silence, not a fallback rendering.
2. **`Message` is not flat.** `Do`, `After`, `When`, `Would`, `ForTarget`,
   `ForInvestigator`, `SkillTestMessage`, `ChaosBagMessage`,
   `InvestigatorMessage`, `DamageMessage` and friends *wrap* the real event. The
   narrator must unwrap, and must distinguish "this is the occurrence" from
   "this is a pre/post hook on the occurrence" — otherwise `When`, `Would` and
   `After` of one event each log it. This is the main design risk in Phase 3.

### Reproducing

```sh
# dev JWT (user 1 is admin on this box); secret is the committed dev default
#   HS256 over {"iss":"arkham","iat":<now>,"jwt":1}
# header must be: Authorization: Token <jwt>

curl -H "Authorization: Token $JWT" \
  "http://127.0.0.1:3002/api/v1/arkham/games/$GAME/export" -o export30.json
curl -H "Authorization: Token $JWT" \
  "http://127.0.0.1:3002/api/v1/arkham/games/$GAME" -o publicgame.json   # has .game.log

arkham-replay export266.json --replay-all --undo 200 --trace --output /dev/null 2>trace.txt
grep -c '^> '      trace.txt   # messages processed
grep -c '^client>' trace.txt   # client messages
```

### A log-ordering bug found on the way (fixed)

`Api/Handler/Arkham/Undo.hs:265` published the post-undo log
`orderBy [desc entries.step, desc entries.id]`, unbounded. Every other log read
is ascending, and the client renders the tail of the list, so **after an undo
the panel showed the game's oldest entries**. Fixed in Phase 1: descending with
a LIMIT, reversed in Haskell.

### Two export bugs found on the way

Both are one root cause and neither is a log problem, but they block exports:

**Step 2619 of this game cannot be decoded.** Its stored queue contains a
`Message` constructor that no longer exists:

```
PersistMarshalError "Couldn't parse field `choice` from table `arkham_steps`.
Error in $.choiceMessages[2]: ... but got EnemyDefeated."
```

`EnemyDefeated` is now only a `MessageType` tag (`Message/Type.hs:14`
`EnemyDefeatedMessage`); the message itself was renamed to `Defeated` /
`EnemyLocationDefeated` (`Message.hs:152-153`). Old rows serialized under the
old name are permanently undecodable.

1. `GET /scenario-export` → **500**, no body.
2. `GET /full-export` → **silently truncated**. It streams
   (`generateFullExportSource`, `Debug.hs:65`), so the exception lands after the
   200 and the response simply stops: 266 of 2,621 steps, invalid JSON, no
   error anywhere. **A partial export is indistinguishable from a complete
   one.** Worth fixing independently of this project — either skip
   undecodable steps with a count in the payload, or emit a trailing
   `"truncated": true`.

Repaired locally by cutting at the last complete step object and appending the
closing `],"log":[],"multiplayerVariant":…}}`, giving a valid 266-step export.

### Still unmeasured

Needs a browser against a running frontend (`npm run dev` was not up):

- [ ] Websocket frame count and bytes during a **scenario setup** — the
      `Shared.hs:141-143` comment says "hundreds of ~100 byte log lines", which
      is structural (`handleMessageLog` broadcasts once per `ClientMessage`,
      `Shared.hs:757`) but has not been counted.
- [ ] `GameMessage.vue` scripting time in a performance trace while a
      log-heavy setup streams in.
