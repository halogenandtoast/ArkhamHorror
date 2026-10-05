# Game log overhaul — journal

**Read this first.** Newest entry at the top. An agent picking this up should be
able to start from the "Next up" line without re-deriving anything.

**Status: Phase 1 landed (model, transport, persistence, renderer). Backend
build GREEN — library, both executables and the 406-module spec suite all link
under `--pedantic`. `npm run tc` passes.**

**VERIFIED END TO END**, migration applied, on a live Core Set game
(`9e63553e`, The Gathering). A real clue discovery produced:

```json
{"tag": "LogRowStructured",
 "contents": {"seq": 1, "kind": "Mechanic",
   "body": [{"tag": "LogI18n", "contents": ["log.discoveredClues",
     {"count": {"tag": "LogNumber", "contents": 1},
      "investigator": {"tag": "LogRefPart", "contents": {
        "kind": "RefInvestigator", "name": "Daisy Walker: The Librarian",
        "cardCode": "c01002", ...}}}]}], ...}}
```

and rendered in the browser as **"Daisy Walker: The Librarian discovers 1 clue"**
— plural branch correct, investigator a hoverable chip, no console errors. The
legacy row beside it (`draws {token:"PlusOne"} chaos token`) rendered through
the ingest parser with its token image intact, proving both paths share one
renderer.

**Measured payload win** on `8572c1df` (2,885-step campaign):

| | before | after |
|---|---|---|
| log field | 481 entries, 51,934 B | 40 rows, 5,493 B (**−89.4%**) |
| whole payload | 252,726 B | 206,343 B (**−18.4% per fetch**) |
| log share | 20.5% | 2.7% |

> **Before running the new code: apply the migration.** The entity now has
> `payload` and `seq` columns, so without `add_payload_to_log_entries` every log
> read fails at runtime. Local DBs are applied by the user.

**Phase 3 started: `Arkham.Log.Narrator` exists and the build is GREEN** —
library and both executables registered under `--pedantic`; `npm run tc`
passes; the API reads fine. A state machine, per the user's design: open on a
message, accumulate while waiting, emit once when it has enough, bail on
anything that stops making sense. First machine is the skill test. Hook widened
(`RunObservers`), wired on the action path and in `arkham-replay --trace`.

**The narrator works, verified in a live game.** An investigate in `9e63553e`
produced, in order:

```
Daisy Walker draws [PlusOne] chaos token     <- legacy row, parsed at ingest
Daisy Walker passes [intellect] by 2         <- NARRATED, kind=Test, seq 4
Daisy Walker discovers 1 clue                <- structured sendLog, kind=Mechanic
```

Stored row:

```json
{"seq": 4, "kind": "Test", "body": [{"tag": "LogI18n", "contents":
  ["log.skillTestPassed", {"by": {"tag":"LogNumber","contents":2},
   "investigator": {"tag":"LogRefPart","contents":{"kind":"RefInvestigator",
     "name":"01002","cardCode":"c01002","entityId":"c01002"}},
   "skill": {"tag":"LogIcon", ...}}]}]}
```

**Exactly one** `Test` row for the test, despite the engine sending five copies
of the result — the unwrapped + `SkillTestInitiatorTarget` pair holds. The
`name` is the bare card code, resolved client-side to "Daisy Walker", which is
the design working as intended. No `send` call exists in any card for this.

**Before adding the next machine, read `ADDING-A-MACHINE.md`.** It exists
because the obvious guess at a message lifecycle was wrong twice for skill
tests, in ways invisible in the type signatures.

**Next up:** verify movement (just fixed), act advance, evade, enemy move,
encounter draw and defeat. Defeat is the suspicious one — an investigator was
defeated in a live game and `InvestigatorDefeated_` produced nothing, which is
the same smell as `EnterLocation`.

### Verified in live play

```
MYTHOS                              <- purple banner
Isabel La Fratta reveals [token]
Isabel La Fratta fails [agi] by 3 against Grasping Hands
Isabel La Fratta takes 3 damage
INVESTIGATION                       <- amber banner
Isabel La Fratta's turn             <- quieter, sentence case
Isabel La Fratta played Cadenza
Isabel La Fratta discovers 1 clue at Study
ENEMY                               <- red banner
UPKEEP                              <- blue banner
Isabel La Fratta gains 1 resource
ROUND 1                             <- gold, biggest break
```

| situation | |
|---|---|
| skill test, all three sentence shapes | ✅ |
| clue discovery with location | ✅ |
| damage and horror, one line | ✅ |
| enemy spawn / engage / attack | ✅ |
| agenda advance (deduped) | ✅ |
| phase banners, in the rail's own tints | ✅ |
| round number | ✅ |
| turn heading | ✅ |
| resources | ✅ |
| chaos token reveal (batched) | ✅ |
| card played | ✅ |
| **custom investigator names** | ✅ |

**The custom investigator bug is fixed and verified**: the code the user saw
raw, `c*7a1f2c9e4b6d48f0a3c5e7d9b1f3a5c70`, now renders as "Isabel La Fratta:
The Pianist".

### Clean-up owed

- **`narrate` is dead code** — the hook calls `narrationFor`. Instrumenting
  `narrate` produces nothing, which already wasted one cycle.
- **`~>` is `infixr 6`**, which collides with `+` (`infixl 6`), so
  `"count" ~> n + 1` does not parse. `infixr 1` would be better.
- **Spawn/engage order reads backwards** — engine ordering, see below.
- 27 legacy `send` sites remain and still work.

### Adding a field to `Game` touches five places

Found one compile at a time; written down so the next person does not:

1. `Game/Base.hs` — the field.
2. `Game/Json.hs` — **both** encoders (`toJSON` *and* `toEncoding`) and the
   parser. The parser must use `.:?` with a default or **every existing save
   fails to decode**.
3. `Game.hs` — the `newGame` construction.
4. `tests/TestImport.hs` — the harness builds a `Game` by hand and `StrictData`
   makes a missing field a compile error.
5. Wherever it is updated — here, `Game/Runner.hs` on `BeginRound`.

---

## 2026-10-05 — Two bugs that only a revisable row could have

The block worked, then two things broke that are worth writing down because
both come from the same wrong assumption: **the log is append-only.** It is
not, any more.

### 1. Every entry published twice

```haskell
let publishLog = lastN gameLogTailSize (oldLogEntries <> updatedLog)
```

Correct while `updatedLog` was only this action's NEW rows. The moment it
became the whole tail -- re-read from the DB so the client sees the result of
the keyed merges -- that line concatenated the previous tail with the full new
one.

The shape of the bug is worth remembering: `lastN 40` then truncates, so a game
with 40+ entries hides it completely and only a FRESH game shows it (4 old + 7
new = 11 rows, first four repeated). A long-running test game cannot observe
either the bug or the fix.

The `Unhandled` branch needed care too: it returned `(g, oldLogEntries, [])`, so
naively publishing `updatedLog` would have blanked the log -- the same failure
as the undo bug earlier in the day. It now returns the old tail in that slot,
which is right: nothing happened, so publish what is already there.

### 2. The block never updated on screen

The server had it perfectly -- `key`, `revisedAt: 12`, `tone: Good`,
`children: 2` -- and the client showed the opening line forever.

`updateGameLog` had an optimisation: if the row count and the last `seq` match,
nothing changed, skip the rebuild. A revision changes **neither**. The block
keeps its seq, the list keeps its length, and the body is rewritten underneath.

Replaced with a signature over `(seq, revisedAt)` for the whole tail. It is
bounded at 40, so walking it is free, and `revisedAt` exists precisely because a
row can change without the list changing.

`revisedAt` also had to be added to the TypeScript type and decoder -- ts.data.json
ignores unknown keys, so the field was arriving and being silently dropped.

### Verified end to end

```
Daisy Walker's turn
  ┌ Daisy Walker reveals [+1]
  │ Daisy Walker discovers 1 clue at Study
  └ Daisy Walker passes [intellect] by 3 investigating Study · 5 vs difficulty 2
```

Green band, collapses to it, and the clue discovery is INSIDE the block -- which
was the last unproven piece of `sendLogDuringTest`.

## 2026-10-05 — The block opens with the test and is revised in place

Asked for: the grouping should start when the test starts and update as it
proceeds, with the consequences (the clue it discovered) inside it.

### Why the previous shape could not do that

Everything was built at the END, from `SkillTest` state, because a narrator
frame cannot survive the four HTTP actions a test spans. The fix is not a
better frame — it is to stop holding state in the narrator at all and let the
**database** be the accumulator.

Two new fields on `LogEntry`:

- `logEntryKey` — a stable identity. A later entry with the same key **revises
  that row**: body, kind and tone are replaced, children are kept, and the row
  keeps its original `step` so the block stays where the test began rather than
  jumping to the bottom of the log.
- `logEntryAttachTo` — the entry is appended to that row's children instead of
  landing beside it.

`writePendingRow` implements both. Two deliberate consequences:

- Rows are stored **one at a time** rather than in one `insertMany_`, so a row
  keyed earlier in the SAME action is there to be revised by a later one. A
  test that opens and resolves without an intervening ask is exactly that case.
- The published tail is **re-read from the DB** instead of built from the
  pending rows. The merges happen in the database, so what the client must see
  is their result, not the sends that produced them.

A key with no row behind it inserts normally. Losing the grouping is a much
smaller failure than losing the line.

### Migration

`add_key_to_log_entries` — nullable `key TEXT`, partial unique index on
`(arkham_game_id, key)`. Partial because only keyed rows are looked up that way
and they are a tiny fraction of the table; unique because two rows sharing a key
within a game is exactly the duplication the key exists to prevent.

**It is not optional.** `ArkhamLogEntry` declares the column, and persistent
names every column in its SELECTs as well as its INSERTs, so without it reading
the log tail fails too.

### `sendLogDuringTest`

Clue discovery and the chaos-bag draw are the two engine sites that `sendLog`
directly. Both now use `sendLogDuringTest`, which attaches to the open test or
sends normally when there is none — the sender does not have to know.

### Verified live, and the one gap

With this binary a real mythos phase reads:

```
1 doom is placed on Rise of the Ghouls
RISE OF THE GHOULS ADVANCES
Isabel La Fratta draws Ghoul Minion
Ghoul Minion engages Isabel La Fratta
Ghoul Minion spawns at Study
Isabel La Fratta triggers a Forced ability on Erlkönig
1 doom is placed on Erlkönig
Ghoul Minion attacks Isabel La Fratta
Isabel La Fratta takes 1 damage and 1 horror from Ghoul Minion
```

**The block still does not open.** A test sat at ST.2 with nothing logged for
it. The cause is that `BeginSkillTestWithPreMessages` is turned into its primed
form by a direct `runMessage`, not a push (`Game/Runner.hs:3192`), so which
shape reaches the queue depends on how the test was begun. Now matched in three
places — both `BeginSkillTest*` shapes and `StartSkillTest_` reading from
state — which is free precisely because the opening is keyed: whichever fires
first creates the block and the rest revise it.

### Known gap

Undo into a half-finished test. `trimLogFrom` deletes rows at or after the
target step, but the block keeps the step it OPENED at, so undoing into the
middle of a test leaves the block with its final result still showing.

## 2026-10-05 — The skill test becomes a block that collapses to its band

Asked for: everything about a test in one block, the stats in a special format,
and the result as a band at the end that the block collapses to.

### `LogTone`

A new optional field on `LogEntry`: `Good` | `Bad`. Deliberately NOT a
`LogKind` — a skill test is a `Test` whether it passed or failed, so the kind
says what sort of event it was and the tone says how it turned out. It is what
lets the client colour the band without parsing the sentence back out of its
i18n key, which is exactly the kind of string-sniffing this overhaul exists to
delete.

It costs nothing to reuse, so a defeat carries one too: an investigator going
down is `Bad`, anything else going down is the point.

### The arithmetic moved into the body

`log.testArithmetic` used to be a child. It is now a part of the parent's
**body**, because the client folds a test down to its band and the numbers are
the half of that band worth keeping when everything else is gone. So
`skillTestDetail` returns `([LogEntry], [LogPart])` — children and stats — and
the result entry takes both.

### Why there is no PASSED/FAILED chip

The obvious thing is a bold uppercase verdict on the left of the band. Tried it
and took it out: the sentence already reads "passes [intellect] by 2", so a
chip is both redundant *and* a second piece of English to keep in step with the
locale. The tone colours the band and nothing else; the word stays in the
sentence, which also means `logEntryToText` — the flat `body` column and the
replay trace — keeps it.

### Shape

```
┌ (open)
│  Isabel La Fratta commits Deduction
│  Revealed [+1], [skull]
├─────────────────────────────────────────────
│ ▸ Isabel La Fratta passes [intellect] by 2 investigating the Study  5 vs difficulty 3
└─────────────────────────────────────────────   ← green band; red when Bad
```

Collapsed it is just that band. The band is the toggle, so the whole block is
one click.

## 2026-10-05 — An ability reads as what it is

"Daisy Walker uses Attic" was wrong twice over: it did not say what kind of
ability, and it fired for the basic actions too.

### The message had to change

The wording comes off `abilityType`, and only `UseAbility` carries the whole
`Ability`. `UseCardAbility` — which the first version matched, precisely
because it is the one pushed *after* the cost is settled — carries just a
source and an index, so the ability would have to be looked back up out of game
state.

The trade is that `UseAbility` fires when the player commits rather than after
payment, so an ability cancelled mid-payment is still logged. It also puts the
activation line *before* its consequences, which is the right way round for a
reader.

### The wording

- paid (`ActionAbility`, `AbilityEffect`, `ServitorAbility`) → "activates an
  ability on {source}"
- `FastAbility'` / the three reaction kinds → "triggers a {fast}/{reaction}
  ability on {source}", carrying the symbol the card prints
- `ForcedAbility`, `ForcedAbilityWithCost` → "triggers a Forced ability"
- `Haunted` → "triggers a Haunted ability"
- `DelayedAbility` / `Objective` / `ForcedWhen` are wrappers; the kind that
  matters is inside, so they recurse
- `Cosmos`, `ConstantAbility` and **`SilentForcedAbility`** say nothing

`SilentForcedAbility` is the interesting one. Its whole purpose is an effect
whose card does *not* print "Forced" — the type's own comment says using it
otherwise "tells the player something untrue about their own card", and the UI
deliberately does not prompt for it. Announcing it as Forced in the log would
be the same lie.

### Icons

The symbol is a `LogIcon` part, not a `{fast}` literal in the locale string.
`LogPart.vue` renders `LogIcon` as `<i class="fast-icon">` already, and the
`{…}` icon escaping that bit `labeledI` never runs over log entries — an
`<i18n-t>` slot would just have been left unsubstituted.

### Basic abilities say nothing

`abilityBasic` (plus `notPlayerAbilityIndex` as a belt) filters out fight,
evade, investigate, move and engage. Those are every action a player takes, and
the log already covers them better further down — as the test they provoke or
the movement they cause. "Daisy Walker activates an ability on the Attic" for
walking into the Attic is noise.

## 2026-10-05 — Nesting, at last, and a dozen more narrations

Two things the user called out: the nested entry the design promised was never
delivered, and none of the "state machine" cases had been hit.

### Nesting without frames

The frames were the wrong shape and the journal said why: a skill test spans
four HTTP actions, and the narrator ref is recreated per action, so a frame
accumulating commits across them is gone by the time the result arrives.

The way through is that **the engine already keeps the state a frame would
have**. `SkillTest` lives for the whole test and holds
`skillTestCommittedCards`, `skillTestRevealedChaosTokens`, the difficulty and
the base value. So the result message builds the whole nested entry at once,
reading back what the test accumulated:

```
Test   Isabel La Fratta passes [intellect] by 2 investigating the Study
  └ Isabel La Fratta commits Deduction
  └ Revealed [+1], [skull]
  └ 5 vs difficulty 3
```

Two narrations had to STOP being top-level for this to not say everything
twice:

- `SkillTestCommitCard_` is no longer narrated on its own. The `oneShot` case is
  kept as a comment so the next reader does not "fix" the omission.
- `Arkham.ChaosBag` skips its reveal line when `sourceIsSkillTest`. Non-test
  draws (Dark Prophecy, an ability that reveals) still log on their own.

Every child is dropped rather than guessed: a test whose difficulty cannot be
calculated, or that ends with no revealed token, simply has one fewer child.

### Nineteen more narrations

Player card draw, discards, resources spent, ability use, location revealed,
clues placed/removed, doom placed/removed, clues placed on your location, surge,
shuffling a discard back in, a search's finds, trauma suffered, taking control
of an asset, something removed from the game, and the three campaign-log writes.

Three of these needed care:

- **`PlaceDoom`, not `PlaceDoomOnAgenda`.** The latter resolves by pushing the
  former at whichever agenda is unflipped (`Scenario/Runner.hs:339`), so
  narrating both logs the mythos phase's doom twice. Matching the general one
  also catches doom landing on an enemy or a location, and names it.

- **Campaign-log keys are not `Show`n.** Every key is wrapped in a per-campaign
  constructor (`ThePathToCarcosaKey HasturHasYouInHisGrasp`), and the locale
  file is keyed by the **contents**, not the tag — so the name comes off the
  JSON encoding. That makes a recorded entry read in the log exactly as it does
  on the campaign-log screen, in whatever language is loaded.
- **`UseCardAbility`, not `UseAbility`.** The latter is the request that still
  has to be paid for; `ActiveCost.hs:1912` pushes the former once the cost is
  settled and the ability was not cancelled.

### Audience is a presentation filter, not a privacy one

The card-draw and search narrations are the first to use `LogAudience`, and
`GameLog.vue` now actually honours it — nothing read the field before.

But be precise about what it buys: **every investigator's `hand` is already in
the payload each client decodes.** An `OnlyPlayer` entry means "this line is
about your hand, not the table's", and it is not a confidentiality boundary.
Anything that genuinely must not reach another seat needs the payload split
per recipient, which the room broadcast does not do today.

## 2026-10-05 — Damage names what caused it

"Would be nice if I take horror or damage, to say what from."

The obstacle: `AssignedDamage Target Int Int` has the right *amounts* and fires
exactly once, but carries no source. The messages that do carry one are all
wrong in some other way:

- `InvestigatorAssignDamage` is the intent, upstream of the `WouldTakeDamage`
  window, so a prevented hit would still be logged.
- `InvestigatorDoAssignDamage` is re-pushed through the distribution loop (it
  accumulates `damageTargets`/`horrorTargets` as it goes), so it fires many
  times per event.
- The narrator cannot hold the source between the two: the distribution ask
  ends the action, and the narrator ref is per action.

So the source is threaded instead. `AssignDamage Target` gains one — it is
pushed from exactly two places, both in `handleCheckDefeated`, which already
has the source in scope — and hands it to `AssignedDamage`. Six pattern matches
across cards and the effect runner needed a new `_`; nothing branches on the
new field, which is why the constructor comment says so out loud.

`FromJSON` takes three shapes for `AssignedDamage` now: the current one, the
pre-source one, and the bare target from before the amounts existed. A save
written before this gets `GameSource`, which only the log ever reads.

The line degrades the same way the rest do: when `sourceRefFor` has no chip for
the source — an upkeep step, the scenario itself — the sentence falls back to
the sourceless key rather than printing something a reader cannot place.

## 2026-10-05 — The log emptied on undo, and chat now outlives one

Reported as "when I undo the log empties out entirely unless I refresh". It is
one line, and it had been there the whole time:

```haskell
publishToRoom gameId $ GameUpdate $ PublicGame gameId arkhamGameName [] arkhamGameCurrentData
```

Single-step undo published an **empty** log. The client replaces its log with
whatever an update carries, so `[]` is not "nothing changed", it is "the log is
now nothing" — and it stayed that way until the next fetch. The multi-step
handler had always refetched the surviving tail; this one never did. It does
now.

Worth remembering generally: the log travels inside `PublicGame`, so every
publisher has to pass a real tail. There is no "omit" value.

### Chat survives an undo

A typed line is not a game event, and rolling the game back does not unsay it.
`trimLogFrom` replaces the three bare `delete`s: it moves chat rows to
`toStep - 1` and deletes the rest of the range. That keeps them in order (they
land exactly at the rollback point, since they sort last within that step) and
out of the range this undo — and every later one — deletes.

Two details that look wrong and are not:

- **Not clamped at 0.** An undo back to step 0 would otherwise land the chat
  *on* 0, and the `>= toStep` delete would take it after all. The column is a
  plain `Int`; a negative step just sorts first.
- **Chat rows are found by decoding payloads, not by `payload ->> 'kind'`.**
  The range is small for every undo but the scenario-wide one, and the raw SQL
  version would silently stop matching the day that field is renamed.

Everything else about undo is unchanged: rows at or after the target still go,
per the user's call ("smallest change; log stays consistent with the game
state").

### Chat UI

Chat draws itself now — a lifted card with a left accent, the speaker above the
words — rather than the inline "Name: text" every other entry shape produces.
The server still builds the body as one sentence so the flat rendering (traces,
legacy clients) reads correctly; `GameLogEntry` takes it back apart.

## 2026-10-05 — Flat panel, and a chat box the rules can hear

Three asks in one: flatten the UI ("it's a container in a container"), add a
chat box, and "make sure if it says Hastur when the hastur ability is active to
trigger it".

### Flattening

The log was three containers deep: the sidebar, a rounded inset card
(`.game-log`, its own background and 10px margin), and a rounded box around
every single entry. Now the log *is* the panel — it fills the sidebar with no
margin, no radius and no second edge — and entries are separated by a hairline
instead of each sitting in a card. It keeps the dark surface, because the
entries are light text and the sidebar is pale in a light theme.

The scroller owns the padding, which is what finally lets a phase banner cancel
it with a negative margin and run genuinely edge to edge. Banners are flat
colour now, per the user: the gradient fade read as a half-drawn row.

### Chat

`ChatMessage InvestigatorId Text` is a real engine message, not a side channel.
That buys three things at once: it is persisted with a step like any other log
row (so "undo back to here" works on it), it reaches the room through the
normal `GameUpdate`, and the rules get a seam they can read. A `Chat` 'LogKind'
renders it as a quote — full width, wrapping, `white-space: pre-wrap`.

The narrator renders it. That looks odd for something that is not derived from
anything, but it is the only way the line lands in the log in message order
with everything around it.

### Hastur

Carcosa's Ultimatum of the Unspeakable Name and of the Brass Crown, and Dark
Matter's Unspeakable Oath, all turn on "spoke, **wrote, or typed** the name".
The engine could only ever hear it through a one-click recorder button in the
scenario bar, which meant remembering to press it.

Now `Arkham.UltimatumsAndBoons` reads `ChatMessage` directly and, when the same
conditions the button checks hold, pushes the *same* message the button pushes
(`InvestigatorAssignDamage iid CampaignSource DamageAny 0 1`) — so the trauma,
the Brass Crown tally and everything else downstream are unchanged. Dark
Matter's version also covers TASSILDA.

### Also

`LogEntry.step` and `LogRowLegacy`'s step are both decoded with `failover` on
the client. A strict decoder turned "server one build behind" into "the entire
log fails to decode", which is exactly what happened here during the rollout.

## 2026-10-05 — Undo back to a log entry

The user's ask: "can the messages track which step they were at so that we can
undo to that point if needed, there should be a mouseover in the log and then a
confirmation if you select undo". Taken now, explicitly because nobody is on
the new logging engine yet, so the wire shape is still free to move.

### What a step means here

`arkham_log_entries.step` already existed and is what undo deletes by. The
numbering is the thing to get right, and it is stated once in `Shared.hs`:

- the game is on step `k`
- an action runs; every row it writes is tagged `step = k`
- the game becomes step `k + 1`, and `ArkhamStep k+1` holds that action's
  patch-down

So an entry tagged `k` is "the game was on `k` just before this happened", and
landing an undo on `k` is exactly "put it back to just before this line". Every
entry one action produced shares a step, so the rewind is always a whole
action — a log entry can never cut an action in half.

### Backend

- `LogEntry` gains `logEntryStep :: Maybe Int`, and `LogRow`'s legacy arm gains
  one beside the text (`LogRowLegacy Text (Maybe Int)`). The legacy arm matters
  more than it looks: almost all of any live game's scrollback predates the
  structured log, and without it the feature would be invisible on exactly the
  games people are playing.
- `toLogRow` stamps the step from the **row's own column**, never from the
  payload. The column is present on every row ever written, including the whole
  history from before the field existed, and it is the thing undo targets.
  `pendingRowToLogRow` does the same for the live frame, so an entry is as
  undoable the moment it appears as it is after a refetch.
- `stepBackToScenarioStep` split in two. The body is now `stepBackToRawStep`,
  which takes an **absolute** game step; the scenario version keeps its old job
  of turning a `gameScenarioSteps` target into a distance back. New entry point
  `stepBackToGameStep`, new route `PUT /undo/step/#Int`.
- Nothing about safety changed: the target is still clamped up to the Epic undo
  floor, membership is still checked by `UniquePlayer`, and a target that is not
  in the past is refused outright, so a stale panel cannot roll a game forward.

### Frontend

- `GameLogEntry.vue` renders a small rewind button, absolutely positioned and
  `opacity: 0` until the row is hovered or the button is focused. It is a
  *sibling* of the headline, not a child — the headline is itself a `<button>`
  once an entry has children, and a nested button is invalid and does not
  click.
- Only a top-level entry offers it. A child shares its parent's step, so a
  second control beside it would promise a finer rewind than the engine has.
- Choosing it raises the existing `Prompt`, naming the line being undone to; the
  flat name is built from the parts, with i18n vars flattened rather than
  resolved a second time.
- `GameLog`'s `canUndo` defaults to **false**, so the replay viewer and a
  spectator get nothing without saying so.

### Also settled this session

- "Taking a move action doesn't show the movement" — `EnterLocation` arrives
  inside `Simultaneously $ Run [...]`, and that branch ran its sub-messages past
  the narrator hook. Fixed in `Arkham.Game`; see the entry below.
- "Gains 1 resource shows up twice" — `TakeResources` now matches the `False`
  variant only. `True` is the resource *action*, whose handler pushes a `False`
  one to do the gaining.
- A live game showed thirteen "reveals" lines with no test result. **Not a log
  bug**: Dark Prophecy (04032) is fast and offers itself at every
  `WouldRevealChaosToken`, and a scripted click loop kept replaying it.

### Verified live

`45577c03` (The Gathering): round banners counting 1→4, phase banners in the
rail's colours, `gains 1 resource` once per upkeep, damage as one line
("takes 1 damage and 1 horror"), spawn, engage, enemy attack, agenda advance.
Spawn still logs *after* engage — engine ordering, not the narrator.

### Next up

- Movement and the undo control still want a live pass.
- Defeat is the remaining suspicious narration: `InvestigatorDefeated_` produced
  nothing in an earlier live game, which is the same smell `EnterLocation` had.
- Coverage: keep adding narrations. 27 legacy `send` sites remain.

## Design decisions (2026-10-05, answered by the user)

1. **Audience: per-player.** Every entry carries
   `audience :: Everyone | OnlyPlayer PlayerId`, persisted, filtered on read.
   Hidden information (you drew an enemy, a revelation only you saw) enters
   history for the first time — today `ClientCardOnly` is dropped outright
   (`Shared.hs:778`).
2. **Verbosity: nesting only, no setting.** Composite events collapse by
   default; the backend always sends everything and the client folds it. No
   stream variants, no settings surface. Revisit only if the collapsed default
   turns out to hide something people need at a glance.
3. **Legacy rows: parse once at ingest.** Pre-migration flat-text rows are
   converted to the part model one time, so old games keep a working log and
   the renderer has exactly one path. This is what lets `GameMessage.vue` be
   deleted outright rather than kept as a second render path.
   - Implementation note: the parser stays in **TypeScript**
     (`legacyLogParse.ts`), run in the store at ingest, not in a render
     function. Rationale: it is a parser for a DSL being deleted, so writing a
     throwaway Haskell copy buys nothing; and the one case that needs game state
     (`GameMessage.vue:50-53` looks up whether a location is revealed) has it in
     the store. The regexes survive in exactly one non-render function that only
     old rows reach.
4. **Nesting: collapsible groups, "live tail" default.** The newest top-level
   entry has its **whole spine open**; it closes when the next entry arrives.
   Everything behind it is folded, with a badge counting what is hidden.
   Mockup of all six options:
   https://claude.ai/artifact/1chPenp5w3xBftaC62dZsU

   Rules, as decided:
   - **Spine, not just the headline.** The open set is the newest top-level
     entry *and every group nested inside it*, so a skill test inside an
     investigate action is readable without a click.
   - **A click pins.** A manual toggle overrides the rule for that one entry
     and survives new entries, so you can study an older test while play
     continues. The override is dropped as soon as it agrees with the rule
     again (so a pinned-open entry stops being pinned once it *is* the tail).
   - **Applied to the batch, not per entry.** Entries arrive one batch per
     action (see Phase 2 batching), so the hand-off is not animated in real
     play: the panel lands with the last event of the action open. Do not add
     per-entry animation to simulate streaming — the engine does not stream.
   - **A leaf entry still closes the previous group.** "Close once the next
     message *or* nested comes in" — any newer top-level entry ends the tail.
   - This replaces the plain collapsed default. Pure client state; **no
     backend consequence.**

---

## Mockup: presentation options

Interactive comparison of five renderings of one real round of The Gathering
(Mythos → Daisy's turn → Enemy phase), in the app's own palette:

https://claude.ai/artifact/1chPenp5w3xBftaC62dZsU

| Variant | Rows at rest | Verdict |
|---|---|---|
| Today | 3 | The baseline. One round, three lines, one of them "clue(s)" |
| Derived, flat | 36 | Complete but rankless — a token modifier sits level with an enemy attack |
| **Live tail** | 14 | Newest event's spine open, rest folded. The chosen default |
| All collapsed | 11 | Shortest, but makes you click the one event you most want explained |
| All expanded | 36 | Hierarchy survives, but fills a 400px sidebar before the turn ends |
| Indented, static | 36 | Visually identical to expanded; the disclosure was never the expensive part |

Takeaway from building it: the work is in getting the **structure into the
data**. Once the narrator emits groups, any of these renderings is a dozen
lines of Vue, so the presentation choice is cheap to change later and should
not block Phase 1.

---

## 2026-10-05 — Damage: three wrong answers, and the one that was right

The investigator damage path cost more than anything else this session. For the
record, so nobody repeats it:

1. **`Damaged_`** — never fires for an investigator.
2. **`PlaceTokens_`** — reached by grepping `Investigator/Runner.hs` for
   `addTokens` and finding exactly one site. Wrong: instrumenting showed **12
   `PlaceTokens` messages across a full enemy attack with damage assigned, every
   one of them Doom or Clue**.
3. The right answer is **`AssignedDamage Target Int Int`**, pushed from
   `handleAssignDamage` in **`Investigator/Runner/Damage.hs:1093`** — a module
   the earlier grep never looked at because it assumed `Runner.hs` was
   authoritative.

It also carries damage and horror **together**, so an attack is one line —
"takes 1 damage and 1 horror" — which is how a player experiences it.

**The lesson is about the grep, not the engine.** `grep` the whole tree, not the
file you assume owns the behaviour. And instrument before theorising: every one
of the three wrong answers survived a round of plausible reasoning and died in
seconds against a trace.

## 2026-10-05 — `narrate` is dead code, and two dead ends

Two things cost a build cycle each and are worth not repeating.

**`narrate` is never called.** When the pure-match optimisation went in, the
`Arkham.Game` hook changed from `narrate ref msg` to calling `narrationFor`
directly, so only a narrating message pays for `runWithEnv`. `narrate` stayed
exported and unused — and then got instrumented, producing zero output, from
which a wrong conclusion was nearly drawn. **Instrument at the hook in
`Arkham/Game.hs`.** Better: delete `narrate`, or move the `runWithEnv` inside it
so there is one path again.

**`Damaged_` is not the investigator damage path.** Grasping Hands put three
damage on Daisy and narrated nothing. `Investigator/Runner.hs:2300` shows the
only site that adds damage tokens is the `PlaceTokens` handler, so the narration
moved to `TokenMessage (PlaceTokens_ …)`, which also covers horror. Still
unverified in play — see the status table at the top.

## 2026-10-05 — Richer lines, and `Arkham.Log.Refs`

The log said "discovers 1 clue" without saying *where*, which is most of why
the line is worth reading. Fixing that needed the monadic ref layer the journal
had flagged as missing.

### `Arkham.Log.Refs`

Turns an id from a message into a chip. `locationRefFor`, `enemyRefFor`,
`assetRefFor`, …, plus `targetRefFor` / `sourceRefFor`, which peel the wrapper
sources (`AbilitySource`, `UseAbilitySource`, `ProxySource`, `IndexedSource`,
`PaymentSource`, `BothSource`) down to something nameable.

**Everything uses `fieldMay`, never `field`.** The narrator runs inside
`runMessages` and must not throw, and an id in a message routinely points at an
entity that has already left play — an enemy just defeated, a location just
replaced. A degraded chip is fine; a crash is a broken game.

**One lookup shape for every entity.** The per-entity `*Name` / `*CardCode`
fields mostly *do not exist* — `AssetName` does, `AssetCardCode` does not;
`TreacheryName` does not. What does exist for all seven types is
`<Entity>Card :: Field X Card`, and a `Card` carries both. So:

```haskell
fromCard :: (HasGame m, Projection a) => LogRefKind -> Field a Card -> EntityId a
         -> (Name -> CardCode -> LogRef) -> m LogRef
```

Six builders are one line each; a new entity kind needs no new lookup logic.
Only `locationRefFor` is bespoke, because it also reads `LocationRevealed` to
decide whether the chip draws the card's back.

### Lines now

- `log.discoveredCluesAt` — "Daisy Walker discovers 1 clue at the Study".
- Skill tests take the target off the message, in **three shapes** so no
  sentence has a hole: action + target ("…by 2 investigating the Study"),
  target but no action ("…against Rotting Remains", a revelation test), or
  neither.
- `Damaged_`, `EnemySpawned_`, `Defeated_` added, chosen off the engine's
  past-tense convention: `DealDamage_` is the intent and can still be
  cancelled, `Damaged_` is what happened.

### A narration may decline after looking

`oneShot` now returns `Maybe (m (Maybe LogEntry))`. The outer `Maybe` is the
pure match (so only a narrating message pays for `runWithEnv`); the inner one
lets a narration give up once it has looked. Damage to something with no
nameable chip now says nothing instead of emitting a **blank row**, which the
first version did.

### Mistake worth recording

I wrote `Arkham/Helpers/Log.hs` with a heredoc without checking the path was
free. It is a **tracked file** — the *campaign* log (`getCampaignLog`,
`hasCampaignOption`) — and I overwrote it. Caught on the next command, restored
with `git checkout`, moved mine to `Arkham.Log.Refs`. Check before writing.

## 2026-10-05 — The frame lifetime bug, and what it changed

Drove a real investigate in `9e63553e` and got **no** log entry. Instrumented
`narrate` to print the decision each step makes. One real skill test:

| decision | count |
|---|---|
| `[0/OPEN]` | 1 |
| `[1/Wait]` | 5 |
| `[1/Ignore]` | 19 |
| **`[0/EMIT]`** | **0** |

Then the result messages arrived — with the stack **empty**.

### Root cause

**A skill test spans four HTTP actions**: open the test, commit cards, draw the
token, apply results. Every ask ends an action. The narrator ref is created per
action in `updateGame`, so the frame opened when the test began is destroyed
before the result arrives.

This is not a bug in the state machine; it is the machine being given a
lifetime shorter than the events it was asked to describe.

### The fan-out, measured exactly

For **one** test the identical payload arrives **five** times:

```
Will  (SkillTestMessage (PassedSkillTest_ ... SkillTestInitiatorTarget ...))
When  (...)
After (...)
       SkillTestMessage (PassedSkillTest_ ... SkillTestInitiatorTarget ...)   <- the real one
Do (After (...))
```

Exactly one is **unwrapped**. Combined with the user's `SkillTestInitiatorTarget`
tip (the engine also sends a copy to every committed skill, the investigator and
every revealed token — 36 copies for three tests in one trace), the pair gives a
trigger that fires **exactly once per test and needs no state at all**.

### Design change — and the frame machinery is gone

- Skill test narration is a **`oneShot`**: emitted from that single message. No
  frame, so the four-action span is irrelevant.
- **The frame machinery was deleted.** Not a change of mind about the design —
  GHC proved the code unreachable. With no machine constructors, `Frame` holds
  an uninhabited `Machine`, so `[Frame]` can only ever be `[]` and the driver's
  frame branch is dead; `-Werror=overlapping-patterns` rejects it. Stubbing
  `step` did not help, because the emptiness of `Machine` is what makes it
  unreachable, not what `step` returns.
- The `Narrator` type, the ref and the hook stay, so re-adding state later
  costs nothing structural.

**The uncomfortable part, stated plainly:** a stack of accumulating frames was
asked for, and nothing could use it. Almost every player-visible event in this
engine spans an ask — the skill test spans four actions, and the investigate
action that contains it spans all four too. Frames only work for events that
resolve inside a single action, and no such event has turned up yet. Whether
one exists is an open question, and worth answering before rebuilding the
machinery.

The alternative, if accumulation really is wanted across actions, is to persist
narrator state with the game rather than per action — which means it has to
diff and undo correctly, and is a much larger commitment. Not taken.

### Two instrument lessons

- **`--replay-all` cannot study a past event.** It reinstalls each step's
  *saved queue* — what remained after the action — so the messages that ran are
  gone. Replaying 8 steps around a completed test showed zero skill-test
  messages. (`ADDING-A-MACHINE.md` recommended it; corrected.)
- **My first instrumentation truncated at 160 chars**, which is before the
  target field, and made `SkillTestInitiatorTarget` look absent when it was
  only cut off. Logging the *decision* rather than the message is what actually
  answered the question.

## 2026-10-05 — Phase 3: the narrator, as a state machine

Design is the user's: *"a simple state machine that can transition on specific
messages; when it gathers enough details it sends the narration, otherwise it
waits; if an unexpected message comes in, either give it a fallback or bail."*
That is the right shape, and the audit's two hard findings are exactly what it
solves — `Message` is not flat, and one player-visible event spans many
messages.

### Shape

- `Narrator` is a **stack of frames**, innermost last. A stack because these
  events nest, and when a frame emits, its entry becomes a **child of the frame
  below** — which is what builds the tree the client folds. The nesting in the
  log model and the nesting in the narrator are the same thing.
- `Machine` is a **plain sum**, one constructor per event, not a closure or an
  existential. Keeps every machine's state visible in one file; adding an event
  is a constructor and a case. Chosen for reviewability over extensibility.
- `Step` is `Ignore | Wait | Emit | Bail`. `Ignore` falls through to the frame
  below, then to the openers. `Bail` is the safe default for a confused frame:
  say nothing rather than guess.
- Two guards so a stuck frame cannot eat an action: `frameBudget` (400 messages
  before a frame is abandoned) and `maxDepth`.

### What the trace taught, which guesswork would not have

**`PassedSkillTest_` fans out to every participant.** In one real trace it
fired **36 times** for three tests — once per committed skill, per investigator,
per revealed chaos token, so each can react. The user confirmed the rule:
**only the copy addressed to `SkillTestInitiatorTarget` is the test's own
result.** A narrator keyed on the constructor alone would have logged a single
test a dozen times. Same for the `Failed` variants.

**Match the unwrapped message.** Of the 10 `InitiatorTarget` copies in that
trace, 4 were inside `Do (After (…))`. `After`, `When` and `Would` carry the
same payload around the real occurrence; treating them as results narrates one
test three times.

**Emit once comes free from the machine.** The frame closes when it emits, so a
duplicate trigger finds no open frame and is dropped. No seen-set needed — this
is the design paying for itself.

**`SkillTestResultsData`** (`SkillTest/Base.hs:125`) carries skill value, icon
value, chaos token value, difficulty, result modifiers and success in one
message. Not used yet; it is the obvious upgrade for a richer test breakdown.

### Names are the client's job now

The narrator reads only messages, so it has no investigator *names*. Rather
than give it game access, `investigatorRefById` puts the card code in
`logRefName` as a fallback and `LogPart.vue` resolves the display name from the
card store. Strictly better than a server-side name: it localizes, and it
follows a card whose identity changed after the entry was written.

### Safety rules this module must keep

1. **Silence is the default** — no catch-all rendering. 64% of messages are
   plumbing; a fallback renderer would bury the log in it.
2. **Emit once** — frames close on emit.
3. **Never break the game** — it runs inside `runMessages`: no throwing, no
   pushing, no dependence on an entity still existing.

### Gotcha: `Arkham/Game.hs-boot`

`runMessages`' signature is declared **twice** — in `Game.hs` and in
`Arkham/Game.hs-boot`. Changing one without the other fails with GHC-11890
("conflicting definitions in the module and its hs-boot file") *after* the
whole library has recompiled, which is a slow way to find out. The boot file
declares `RunObservers` abstractly (`data RunObservers`), which is all a
SOURCE importer needs to spell the signature.

### Known rough edges

- Only one machine (skill test). The whole point is breadth; enemy attacks,
  damage, clue discovery, spawns and draws are next.
- hlint wants `Machine` to be a `newtype`. Deliberately `data` — more
  constructors are coming.
- The test harness passes `noRunObservers`, so specs narrate nothing unless
  they opt in. Intentional: specs assert on game state.
- **`Simultaneously` is not narrated inside.** `runMessages` runs each of its
  sub-messages through the pipeline directly, bypassing the hook, so only the
  `Simultaneously` wrapper itself reaches the narrator. Nothing needs it yet;
  remember it before narrating anything that resolves simultaneously.
- The undo path (`Shared.hs:1042`) passes `noRunObservers` on purpose — a
  replayed step must not re-log what is already in history.

## 2026-10-05 — Phase 1: model, transport, persistence, renderer

### Plan change, recorded on purpose

The design called for a batched `GameLogEntries` websocket frame per action.
**Dropped it.** Building it surfaced that `GameUpdate` already carries the log
inside the game payload and is published immediately behind it, so every entry
would arrive twice and the client would need a reconciliation rule to tell the
duplicate from a genuine append.

Instead the game payload carries the log, now as a **bounded tail of structured
rows**. That is strictly simpler — one channel, no ordering question, no
duplicate — and it folds in the Phase 2 payload fix, which was going to have to
change this same field anyway. It also means the log and the board move
together instead of the log racing ahead of the state that explains it.

Consequence worth knowing: there is no longer any per-line log frame. The
"hundreds of separately deflated ~100 byte frames during setup" problem is
fixed for structured entries as a side effect, and remains only for the legacy
`ClientText` path until those call sites migrate.

### Backend

- **`Arkham.Log.Entry`** — the wire model. Deliberately a leaf: it imports only
  the id types, because `Arkham.Classes.GameLogger` imports it and almost
  everything imports *that*. `Arkham.Name`, `Arkham.SkillType` and
  `Arkham.ChaosToken.Types` all import `GameLogger` for their
  `ToGameLoggerFormat` instance, so using those types here would cycle — hence
  names, token faces and skill icons travel as `Text`.
- **`LogRef` is one record with a `kind`**, not a constructor per kind. The old
  DSL grew a separate shape per kind and then a second *arity* for locations,
  which the renderer had to try in order. A new kind now changes nothing on the
  wire; type safety lives in the smart constructors instead.
- **`LogRow = LogRowStructured LogEntry | LogRowLegacy Text`** — history is
  mixed and the server says which shape each row is.
- **`Arkham.Log`** — the typed layer: `ToLogPart`/`ToLogRef`, ref constructors,
  entry builders per kind, `withChildren`, `because`, `forPlayer`, `sendLog`.
  Pure; the monadic grouping DSL waits until the narrator shows what it needs.
- **`ClientLogEntry LogEntry`** added to `ClientMessage`. It is accumulated, not
  broadcast.
- **Persistence**: `arkham_log_entries` gains nullable `payload JSONB` and
  `seq INTEGER`, plus `idx_arkham_log_entry_gameid_seq`. Migration
  `add_payload_to_log_entries`. `step` is untouched — undo deletes by it
  (`Undo.hs:132/180/267/385`).
- **`PendingLogRow`** in `Shared.hs`: legacy text and structured entries
  accumulate in **one ordered list**, not two. Two lists would have reordered a
  mixed action into "all the legacy lines, then all the structured ones".
- **Bounded reads**: `getGameLogTail` replaces `getGameLog` on every request
  path (`Games.hs` ×2, `Admin.hs`, `Shared.hs`). `getGameLog` survives for
  nothing on the hot path now.

### Frontend

- **`GameMessage.vue` deleted.** Its regexes live on in exactly one place,
  `legacyLogParse.ts`, which runs at ingest and never in a render function.
- `LogPart.vue` renders a part. A `LogI18n` part goes through `<i18n-t>` with
  dynamic slots, which is what finally lets a localized sentence carry card
  chips — the thing the old system made you choose between.
- `GameLogEntry.vue` is recursive, keyed by index path, with a `<button>`
  headline so the disclosure is keyboard-operable for free.
- `GameLog.vue` owns the live-tail rule and the pins, and windows the list to 40.
- `Game.vue`: `gameLog` is now `LogEntry[]`, and the full-history copy on every
  update is gone.

`npm run tc` passes.

### First migration, and what it proved

`Arkham/Investigator/Runner.hs` clue discovery, the line that had degraded to
`"discovered clue(s)"`:

```haskell
sendLog
  $ mechanic
    [ ikeyPart
        "log.discoveredClues"
        ["investigator" ~> investigatorRef a.id (toName a), "count" ~> clueCount]
    ]
```

The count is back, the investigator is a hoverable chip, and the sentence lives
in `log.json` where a translator can reach it. That one call site is the whole
argument for the model in six lines.

It also forced **pluralization** into the renderer: a `count` variable now
drives `<i18n-t :plural>`, matching the backend's `countVar` convention. Without
it a message written with vue-i18n's `|` branches silently picks neither.

### A bug the real output caught

The first persisted entry shipped `"entityId": "\"01002\""` — **doubly
quoted**. `investigatorRef` used `tshow`, and `CardCode` derives `Show` from
`Text`, so the quotes came along. The old brace DSL got away with the same
`tshow` only because those spurious quotes doubled as its field delimiters.

Fixed at the root with `idText :: ToJSON a => a -> Text`, which takes the id's
**JSON** form — the same string that keys `game.investigators`, `game.enemies`
and friends on the client, so a ref can be looked up there with no massaging.
Every ref constructor now takes the id itself rather than pre-rendered text, so
the mistake is no longer expressible.

Worth knowing: UUID-backed ids (`EnemyId`, `LocationId`, `AssetId`) were fine
under `tshow`; only the `Text`-backed ones were wrong. That is exactly the kind
of inconsistency that makes a bug sit unnoticed.

One row in dev game `9e63553e` still carries the bad `entityId`. Harmless — the
renderer prefers `cardCode` — and not worth a data migration.

### A pre-existing bug fixed on the way

`Api/Handler/Arkham/Undo.hs:265` read the log `orderBy [desc step, desc id]`
and published it unbounded. Every other log read is ascending, and the client
renders the *end* of the list — so after an undo the panel showed the game's
**oldest** entries. It now reads descending with a LIMIT and reverses, which
fixes the order and bounds the payload at the same time.

### Snags hit, for whoever follows

- `--pedantic` means `-Werror`, so an unused import fails the build. Two did.
- **Adding a module forces a near-full rebuild** (8,639 modules): the `.cabal`
  changing invalidates everything. Batch new modules into one cabal touch.
  `hpack` on this machine (0.37.0) is **older** than whatever generated the
  cabal and refuses to run, so new modules must be inserted into
  `exposed-modules` by hand, in sorted order.
- **Persistent does not generate `HasField`**, so `entity.payload` does not
  work — use `arkhamLogEntryPayload`. Plain records do get record-dot.
- `max_` over a nullable column needs `joinV` to collapse the double `Maybe`.
- ClassyPrelude has no list `takeEnd`, and `Data.Text`'s shadows the name.
- `newLogEntry` was already taken by the DB-row constructor in
  `Api.Arkham.Helpers`; the smart constructor is `mkLogEntry`.
- `logRef` (the `Arkham.Log.Entry` builder) shadowed a local `IORef` named
  `logRef` in `Shared.hs`, and `-Werror=name-shadowing` catches that. The local
  is now `pendingLogRef`.
- **`Arkham.Log` is an umbrella re-export** (`module Arkham.Log (module
  Arkham.Log, module Arkham.Log.Entry)`), which is the pattern
  `project_dev_rebuild_cascade_is_export_hash_not_unfoldings` warns about:
  adding any helper to it bumps its export hash and rebuilds every importer.
  Acceptable here because a card author's default is to import nothing — the
  narrator is the main consumer — but import it with an **explicit list** from
  card modules (as `Arkham/Investigator/Runner.hs` does) and think twice before
  adding to it once the narrator is widely imported.

## 2026-10-05 — Baselines measured

The user supplied a real game: `8572c1df-440e-4f55-a136-90c66d962cd5`, The
Innsmouth Conspiracy, 2 investigators, **2,885 steps**. Numbers and repro
commands in `FINDINGS.md`. The three that matter:

**1. The log is a chaos-token ticker.** 481 entries for the whole campaign, and
98% of them are three sentences: 60.3% "X draws [token] chaos token", 22.2% "X
played Y", 15.4% "X discovered N clue". Damage, horror, enemy attacks, spawns,
evades, fights, encounter draws, act/agenda advancement, XP, skill-test results,
resource gain and movement produce **nothing**.

**2. 3,111 engine messages → 1 log line.** Replaying 2 steps with `--trace`.
The single-step trace is the best possible argument for the whole project:
`ReportXp` goes by carrying `XpDetail {source, sourceName, amount}` for four
victory-display cards, then `GainXP … 4` for each investigator — and the player
is told only that a campaign-log key was recorded. The structured data already
exists; the log discards it.

**3. The log is 20.5% of every game fetch and 97.9% of that is thrown away.**
51,934 bytes of log in a 252,726-byte payload — the second-largest field in
`PublicGame`, bigger than every investigator combined. `GameLog.vue` renders the
last 10 entries, so 50,959 bytes are discarded per fetch, across five fetch
sites.

### Design corrections forced by the measurement

- **`Message` is not flat, and the narrator cannot just `case` on the top-level
  constructor.** `SkillTestMessage` (149), `Do` (621), `After` (83),
  `MoveWithSkillTest` (114), `ForTarget`, `ForInvestigator`, `ChaosBagMessage`,
  `DamageMessage` all *wrap* the real event. The narrator must unwrap, and must
  tell "this is the occurrence" from "this is a pre/post hook on it" — or
  `When`, `Would` and `After` of one event each log it three times. **This is
  the main design risk in Phase 3** and was not in the original design.
- **~64% of messages are pure plumbing** (`Do`, `CheckWindows`,
  `EndCheckWindow`, `ClearUI`, `Ask`, `WindowAsk`, `ResolveWindowInitiations`,
  `SetActiveInvestigator`). The narrator's default case must be **silence**. Do
  not give it a fallback rendering "so nothing is missed" — that would bury the
  log in noise far worse than today.
- **The renderer parses for refs that never occur.** Across 481 entries:
  `investigator` 474, `token` 356, `card` 107, `enemy` **0**, `location`
  **0**. Three of `GameMessage.vue`'s seven regex branches never match, and run
  on every fragment of every render anyway.
- **The clue count is a confirmed regression, not a historical limitation.** All
  74 stored clue entries use the old `discovered 1 clue`; zero use the current
  `discovered clue(s)`. The number was there and the DSL lost it.
- The 60/22/15 content split also means **Phase 3 should start with skill tests,
  damage and enemy attacks**, not with the three things already covered.

### Incidental: two export bugs, same root cause

Step 2619 holds a queue serialized with `EnemyDefeated`, a `Message`
constructor that no longer exists (now `Defeated` / `EnemyLocationDefeated`;
only the `EnemyDefeatedMessage` *type tag* survives). Consequences:
`/scenario-export` 500s, and `/full-export` **silently truncates** — it streams,
so the exception lands after the 200 and the response just stops at 266 of
2,621 steps with invalid JSON and no error. A partial export is
indistinguishable from a complete one. Reported to the user; not part of this
project.

## 2026-10-05 — Live tail chosen over plain collapsed

The user's refinement, and it is the right one: plain collapsed puts the click
cost on *the event that just happened*, which is the one case where the detail
is always wanted. Live tail is the same variant with that case fixed, at a cost
of one predicate.

Added it as a sixth variant with a stepper, because the behaviour is invisible
in a still frame. Stepping through surfaced three things that had to be decided
rather than assumed, all now written into the decision above: the open set is
the whole **spine** (not the headline alone), a manual click has to **pin**
against the rule, and the rule applies to the **batch** rather than per entry —
the engine sends one batch per action, so there is nothing to animate and
pretending otherwise would be a lie about how the game resolves.

At rest: 14 rows against 11 for plain collapsed and 36 for flat. The extra
three rows are the whole cost.

## 2026-10-05 — Mockup for the nesting decision

Built the five-variant comparison above rather than taking the collapse default
on trust. Two things it settled:

- The flat variant is **36 rows for one round of solo play**. That is the real
  argument for grouping, and it is not a matter of taste.
- The three structured variants differ only in `hidden` on a child list. The
  decision is reversible for the cost of one line, so it must not gate the
  backend work.

Content is a genuine Core Set sequence (Rotting Remains, Magnifying Glass, two
investigations of the Study, Trapped advancing, Ghoul Priest attacking) so the
row counts are honest rather than illustrative.

## 2026-10-05 — Phase 0: audit

Read the whole path end to end: `Arkham.Classes.GameLogger` →
`handleMessageLog` → `arkham_log_entries` / `PublicGame` → `Game.vue` →
`GameLog.vue` → `GameMessage.vue`. Full evidence in `FINDINGS.md`; design in
`README.md`.

The decisive finding: **39 log call sites against 5,634 card files (0.7%)**.
That settles the strategy question — the log has to be *derived* from the
message stream, not authored per card. `Arkham/Game.hs:6800` already fires a
hook for every message popped, which is the seam.

Second finding worth calling out, because it is the whole problem in one place —
`Arkham/Investigator/Runner.hs:1576`:

```haskell
-- send $ format a <> " discovered " <> pluralize clueCount "clue"
send $ format a <> " discovered clue(s)"
```

The count was available, the string DSL made it awkward, and the line degraded
to "clue(s)". A structured entry cannot degrade this way.

Also confirmed, all verified in source rather than assumed:

- One websocket frame per log line, hundreds during setup (`Shared.hs:757`).
- The **entire** log ships on every game fetch (`Game.hs:699`) and the client
  renders the **last 10** (`GameLog.vue:14`).
- `getGameLog` has no `LIMIT` (`Api/Arkham/Helpers.hs:67`).
- Everything that isn't `ClientText` is live-only and never persisted
  (`Shared.hs:772`), so card draws, reveals and enemy draws have no history.
- `GameMessage.vue` runs seven regex test+match pairs per fragment per render,
  with no memoization, plus a `.replace()` patching rows from an older build.
- `step` on `arkham_log_entries` is load-bearing for undo
  (`Undo.hs:132/180/267/385`) and must survive any reshape.
- `en/log.json` has **three** keys; the rest of the log is English in Haskell.

### Decisions taken

- **Derive, don't author.** The narrator is one `case` over `Message` in
  `Arkham.Log.Narrator`. A card author's default is to write nothing.
- **Per-card authoring annotates, it does not replace.** The `reason`
  combinator adds a child to what the engine just logged.
- **i18n templates take rich `LogPart` vars.** Retires the choice between
  "localized" and "has card chips", which is why most call sites today are raw
  English.
- **Phase 1 is behaviour-neutral.** Replace the model, transport, persistence
  and renderer while emitting exactly what `send` emits today. Do not add
  narration in the same change.
- **`seq` for identity, `step` retained.** `seq` is monotonic per game and gives
  the client append-only identity and scrollback a cursor; `step` stays on the
  row for undo.
- **Tone is a hard rule, enforced in review.** Terse, present tense, no filler.
  All English in `log.json`.

### Not done

- No baselines measured — blocked on a game export, none checked in. The
  measurement list is at the bottom of `FINDINGS.md`. Take these **before**
  Phase 2 or the perf work has nothing to claim against.
- No code written. Nothing in this overhaul is in the tree yet.
