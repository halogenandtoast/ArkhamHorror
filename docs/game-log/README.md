# Game log overhaul

Living design + progress journal. **Any agent picking this up: read
`STATE-OF-PLAY.md` first for where the work actually stands, then this file for
why.**

- `STATE-OF-PLAY.md` — current status, the model as built, and the invariants
  that will bite you. Supersedes `JOURNAL.md`'s closing status.
- `README.md` (this file) — the design and the reasoning behind it.
- `JOURNAL.md` — phase-by-phase status, what is done, what is next, decisions taken.
- `FINDINGS.md` — the grounded audit of the current system (file:line), kept as evidence.
- `ADDING-A-MACHINE.md` — how to teach the narrator a new event. **Read it
  before adding one**; it starts with "observe the message lifecycle first",
  which is the step that looks skippable and is not.

## The goal

Turn the in-game log from a thin, mostly-empty ticker into a log a player can
read to understand *what happened and why* — including large composite events
(a skill test, an enemy attack, a mythos card resolving) as single readable
units.

Hard constraint on voice: **terse, present tense, no filler.** "Daisy fails by
2." never "Unfortunately, Daisy was unable to succeed…". Every English string
lives in `frontend/src/locales/en/log.json` so the whole corpus is reviewable
by a human in one file.

## The four problems, and the shape of each fix

### 1. Almost nothing logs (coverage)

39 real log-line call sites against ~5,634 card implementation files — **0.7%**.
A strategy of "add `send` to each card" is not reachable and never will be.

**Fix: derive the log from the message stream, don't author it per card.**
Every state change in the engine already passes through `Message`, and
`Arkham/Game.hs:6800` already fires a hook for every message popped:

```haskell
for_ mLogger $ liftIO . ($ msg)
```

A *narrator* that observes that stream and knows what each `Message` means in
English gets the mechanical baseline for free, in one reviewable file, instead
of 5,634 files each having to remember. Per-card authoring then only supplies
the exceptions: flavour, and the "why" a generic message cannot know.

That inverts the default: a card author writes **nothing** and the card still
logs correctly.

### 2. The authoring interface is awkward

Today an entry is `Text`, built by splicing two string mini-languages:

- a brace DSL for rich refs — `Arkham/Card.hs:636` emits
  `{card:"name":CODE:"id"}`, with hand-escaping (`T.replace "\"" "\\\""`);
- an embedded i18n DSL — `$some.key var=s:"value"` (`Arkham/I18n.hs:36`).

They compose only by string concatenation, the client re-parses them with seven
regexes, and nothing is type-checked. Call sites read like
`send $ format (toCard attrs) <> " removed all copies of " <> format card <> " from the game"`.

**Fix: a structured entry built from typed parts** (below), plus combinators so
the common cases are one line and the compiler catches the rest.

### 3. The Vue rendering is odd

`frontend/src/arkham/components/GameMessage.vue` is a `render()` function that
regex-splits a flat string and hand-builds nodes with `h()`, including a
`.replace()` that patches up log rows written before a formatting bug was
fixed. Seven `test`+`match` regex pairs run per fragment, on every render.

**Fix: delete it.** Structured parts render as an ordinary SFC: `v-for` over
parts, one small `<LogPart>` component per tag. Legacy flat-text rows run
through the old parser **once, at ingest**, producing the same part model — so
the renderer has exactly one path.

### 4. Performance, both ends

Backend:
- `handleMessageLog` (`Api/Handler/Arkham/Games/Shared.hs:750`) broadcasts **one
  websocket frame per log line** — hundreds during scenario setup, each
  separately deflated. The surrounding comment already identifies this cost.
- `PublicGame gid Text [Text] Game` (`Arkham/Game.hs:699`) ships the **entire
  log** on every game fetch, and the client renders the **last 10**
  (`GameLog.vue:14`). The whole transfer is waste today.
- `getGameLog` (`Api/Arkham/Helpers.hs:67`) selects every row for the game, no
  limit.

Frontend:
- `updateGameLog` (`Game.vue:710`) copies and freezes the whole array on every
  update; the socket append does `[...gameLog.value, contents]` (`Game.vue:1370`)
  — O(n) per line, O(n²) across a setup.
- `GameLog.vue` watches a computed slice with `{deep: true}` and scrolls with a
  magic `scrollTop + 100`.

**Fix:** batch entries into one frame per action; ship a bounded tail plus a
count and page scrollback on demand; append-only client store keyed by a
monotonic `seq`; windowed list.

## The entry model

One shared contract, in `Arkham.Log.Entry`, serialized to the client as-is.

```haskell
data LogEntry = LogEntry
  { seq      :: Int             -- monotonic per game; the client's identity + ordering
  , step     :: Maybe Int       -- the game step to undo to; see "Undo back to an entry"
  , kind     :: LogKind
  , body     :: [LogPart]       -- the line itself
  , source   :: Maybe LogRef    -- what caused it: answers "why" without prose
  , children :: [LogEntry]      -- nested detail; this is what makes big events readable
  , audience :: LogAudience     -- Everyone | OnlyPlayer PlayerId
  , context  :: LogContext      -- round / phase / turn
  }

data LogKind
  = Structure   -- "Round 2", "Mythos phase", "Daisy's turn"
  | Action      -- a player did something
  | Mechanic    -- engine consequence: damage, clues, spawn, draw
  | Test        -- a skill test, as one group
  | Narrative   -- flavour / story text
  | Record      -- campaign- and scenario-log writes
  | Notice      -- "ignored", "cannot", "no effect"
  | Problem     -- errors, custom-card issues

data LogPart
  = Lit Text
  | I18n Scope (Map Text LogPart)  -- template + RICH vars, recursively
  | Ref LogRef                     -- a chip the client renders and hovers
  | Num Int
  | Delta Int                      -- rendered "+2" / "-1"
  | Token ChaosTokenFace
  | Icon SkillType

data LogRef
  = CardRef Name CardCode CardId Bool            -- faceDown
  | InvestigatorRef InvestigatorId Name
  | EnemyRef EnemyId CardCode Name
  | LocationRef LocationId CardCode Name Bool    -- revealed
  | AssetRef AssetId CardCode Name
  | AbilityRef Source Int
```

Why each piece earns its place:

- `LogPart` retires both string DSLs and all hand-escaping.
- `I18n` taking `LogPart` **vars** is the key move: a template like
  `"{investigator} discovers {count} clues at {location}"` localizes *and*
  keeps its chips. Today you must choose between the two.
- `children` is where "cover large chunks of information" comes from. A skill
  test is **one** entry holding committed cards, revealed tokens, modifiers and
  result — not eight flat lines a player has to reassemble.
- `source` answers "why" uniformly, as data, so the author never writes it into
  the sentence.
- `audience` lets hidden information be logged **at all**. Today
  `ClientCardOnly` is dropped from the persisted log outright
  (`Shared.hs:778`), so "you drew an enemy" can never appear in history.
- `seq` gives the client stable identity for append-only updates, and gives
  scrollback a cursor. `step` stays on the row because undo deletes by it
  (`Api/Handler/Arkham/Undo.hs:132`), and now rides out to the client too so a
  reader can rewind to a line.

## Undo back to an entry

Every entry knows the game step it was written under, and the log offers
"undo back to here" on hover.

The numbering is the whole trick, and it is stated once in `Shared.hs`: the game
is on step `k`, an action runs and tags every row it writes with `k`, then the
game becomes `k + 1`. So an entry tagged `k` means "the game was on `k` just
before this", and landing an undo on `k` puts it back to exactly there. Every
entry one action produced shares a step, so a rewind is always a whole action.

- The step comes from the **row's column**, not the payload (`toLogRow`). The
  column exists on every row ever written, including all the history that
  predates the structured log, which is why a legacy row carries one too.
- `PUT /undo/step/#Int` is the endpoint. It shares `stepBackToRawStep` with
  scenario/turn/phase/round undo, so the Epic undo floor, the membership check
  and the "not in the past" refusal all apply unchanged.
- The control is hidden until the row is hovered or focused, lives only on
  top-level entries, and raises a confirmation naming the line. `GameLog`'s
  `canUndo` is **false** by default, so the replay viewer shows nothing.

## The narrator

`Arkham.Log.Narrator` — one `case` over `Message`, hooked at `Game.hs:6800`.

`mLogger` widens from `Maybe (Message -> IO ())` to a small record of pre/post
hooks in the game monad `m`, so the narrator can read state (names, before/after
values). `collectFromRun` in `Shared.hs:483` adapts trivially.

Grouping is a stack: messages that open a logical unit (`BeginSkillTest`,
`BeginTurn`, `Begin phase`, `EnemyAttack`, a revelation) push a group, their
terminator pops it, and everything narrated between lands as children. Nesting
and "why" therefore come from **one** place rather than from discipline spread
across the card corpus.

This file will be large and that is correct: it is a single reviewable
dictionary from engine message to English.

## The authoring interface

For the cases the narrator cannot know. Shapes to land on (names provisional):

```haskell
logs    :: HasGameLog m => [LogPart] -> m ()   -- one line
logsI   :: (HasI18n, HasGameLog m) => Scope -> m ()  -- i18n key, honours ?scope/?scopeVars
because :: m a -> Source -> m a                -- attach the cause
detail  :: m a -> m a                          -- nest under the open group
group   :: [LogPart] -> m a -> m a             -- open an explicit group
reason  :: Scope -> m ()                       -- add a child to what the engine JUST logged
```

With `ToLogPart` / `ToLogRef` instances, `withVar "card" card $ logsI "banished"`
makes `card` a chip rather than a quoted string. `reason` is the important one
for card authors: it annotates the narrator's line instead of replacing it.

## Writing a log entry by hand

The narrator covers what the engine can infer. A campaign's one-off moments —
a specific card banished, a resident who refuses to speak, a reading that only
this scenario has — are exactly what it cannot, so **direct sending is a
first-class path, not a leftover.**

Scenario and campaign code already works inside a `HasI18n` scope, so the short
form honours it:

```haskell
scenarioI18n $ withVar "card" (String card) $ sendLogI18n Narrative "messages.banished"
```

`ikeyScoped` resolves the key against the ambient `?scope` and folds in whatever
`withVar` / `countVar` put in `?scopeVars`, so an existing call converts by
swapping `send $ … ikey'` for `sendLogI18n`. For anything richer, build the
entry:

```haskell
loc <- locationRefFor lid
sendLog $ mechanic
  [ikeyPart "log.discoveredCluesAt" ["investigator" ~> who, "count" ~> n, "location" ~> loc]]
```

Pick the `LogKind` deliberately — `Narrative` renders as prose, `Notice` is
subordinate, `Record` is a campaign-log write. The kinds are what let the client
style and group entries.

The **27 remaining legacy `send` sites still work unchanged**; `ClientText`
is untouched. They produce flat rows the client parses at ingest, so there is no
rush to convert them — only a reason to prefer `sendLog` for anything new.

## Verification

`arkham-replay --trace` already prints every `ClientMessage`
(`app-replay/Main.hs:244`). Add a `--log` mode that renders the *log* for a
replayed game as text, so:

- a reviewer can read a whole scenario's narrative as a diff-able file;
- a test can assert on it;
- the core-set card pass has an objective target.

## Phases

| # | Phase | Outcome |
|---|-------|---------|
| 0 | Audit + baselines | `FINDINGS.md`, measured numbers for setup frame count, log payload size, render cost |
| 1 | Entry model + transport + renderer | Structured entries end to end, emitting exactly what `send` emits today. Behaviour-neutral; proves the pipe. `GameMessage.vue` deleted |
| 2 | Performance | Batched frames, bounded tail + paged scrollback, append-only client store, windowed list. Re-measure against Phase 0 |
| 3 | Narrator breadth | The ~60 messages that cover the bulk of play |
| 4 | Grouping, nesting, "why" | Collapsible composite events; modifier and source breadcrumbs |
| 5 | Core-set card pass | Core + Revised Core audited against `arkham-replay --log`; `reason` lines where generic narration is insufficient |
| 6 | Sweep + document | Remaining `send` sites migrated; authoring guide in CLAUDE.md |

Phase 1 is deliberately behaviour-neutral. It is the only way to replace two
string DSLs, a regex renderer, and a persistence shape without also debugging
new content at the same time.
