# Game log — state of play

**As of 2026-10-06. This file is the authoritative status.** `JOURNAL.md` still
ends with "No code written. Nothing in this overhaul is in the tree yet", which
was true when it was written and is now badly wrong: Phase 1 shipped, most of
Phase 2 shipped, and several things that were never in the plan shipped too.
Read this file for *what exists*, `README.md` for *why the design is what it
is*, `ADDING-A-MACHINE.md` before adding a narration, and `FINDINGS.md` for the
original audit (its file:line evidence about the **old** system is historical —
most of it has been replaced).

Everything described here is on `main` unless a section says otherwise.

---

## 1. What exists

The log is **derived from the message stream**, not authored per card. One
`case` over `Message` in `Arkham/Log/Narrator.hs` (~60 narrations) produces
structured entries; cards write nothing.

Shipped beyond the original four-problem plan:

| Feature | Where |
| --- | --- |
| Structured entries, typed parts, i18n-first | `Arkham/Log/Entry.hs`, `Arkham/Log.hs` |
| Narration from the message stream | `Arkham/Log/Narrator.hs` |
| Groups (a skill test as one block) | `LogGroup`, `LogGroupBlock.vue` |
| Chat — players type into the log | `LogKind.Chat`, `ChatMessage` message |
| Undo to a specific entry | `logEntryStep`, `Undo.hs:203 trimLogFrom` |
| Retraction (commit → uncommit) | `logEntryTag`, `ClientRetractLog` |
| Scrollback paging | route `/log/before/#Int`, cursor on `seq` |
| Damage/horror naming its source | `logEntrySource`, `sourceRefFor` |
| Cost attribution ("+1 action from Frozen in Fear") | `AdditionalCostPaid`, the narrator hold |
| Investigator class colours | `LogPart.vue` `refClass` |

---

## 2. The wire model — `Arkham/Log/Entry.hs`

```haskell
data LogEntry = LogEntry
  { logEntrySeq      :: Int            -- monotonic per game; client identity + scrollback cursor
  , logEntryStep     :: Maybe Int      -- undo target; load-bearing, see §5
  , logEntryKind     :: LogKind
  , logEntryBody     :: [LogPart]
  , logEntrySource   :: Maybe LogRef   -- "why", as data
  , logEntryChildren :: [LogEntry]
  , logEntryAudience :: LogAudience    -- Everyone | OnlyPlayer PlayerId
  , logEntryContext  :: LogContext     -- round / phase / turn
  , logEntryTone     :: Maybe LogTone  -- Good | Bad; colour without parsing keys
  , logEntryGroup    :: Maybe LogGroup
  , logEntryTag      :: Maybe Text     -- name for later retraction
  }
```

**`LogKind`** — `Structure`, `Action`, `Mechanic`, `Test`, `Narrative`,
`Record`, `Notice`, `Problem`, `Chat`.

**`LogPart`** — `LogText`, `LogI18n` (key + named vars, each itself a part),
`LogRefPart`, `LogNumber`, `LogDelta` (drawn `+2`/`-1`), `LogToken`, `LogIcon`,
`LogCampaignKey` (raw key, resolved client-side — see §8.7), `LogList` (joined
by the locale's list rule).

**`LogRef`** is a clickable chip: kind (`RefInvestigator`, `RefEnemy`,
`RefLocation`, `RefAct`, …), name, card code, entity id, face-down flag.

**Groups** are the user's own design and the reason this works:

```haskell
data LogGroup = LogGroup { logGroupId :: Text, logGroupRole :: LogGroupRole }
data LogGroupRole = GroupHeader | GroupMember | GroupSummary
```

Rows stay **flat and append-only**. A message carries a group id (for a skill
test, the test's id via `skillTestLogKey`), and the *renderer* collects a run of
rows sharing an id into one block. Nothing on the server ever reaches back and
edits an earlier row. See §8.1 — this is the single most important invariant in
the system.

---

## 3. Making an entry

**Builders** — `Arkham/Log.hs`. One per kind (`action`, `mechanic`, `test`,
`notice`, `record`, `chat`, …), then modifiers: `toned`, `because`,
`withChildren`, `forPlayer`, `tagged`, `opensGroup` / `inGroupOf` /
`closesGroup`. Parts: `lit`, `num`, `delta`, `ikeyPart key [("var", part)]`.

**Refs** — `Arkham/Log/Refs.hs`. `investigatorRefFor`, `enemyRefFor`,
`locationRefFor`, …, plus `targetRefFor` / `sourceRefFor` (both `Maybe` — a
source with no chip a reader would recognise is dropped rather than printed
raw), and `byCardCodeRef` for an entity that no longer exists (an act is
*replaced* on advance, so `fieldMay ActCard` returns `Nothing`).

**The narrator** — `Arkham/Log/Narrator.hs`:

```haskell
narrationFor :: Message -> Maybe (m (Maybe LogEntry))
```

The `Maybe` match is **pure**, so a message that does not narrate never pays for
a `runWithEnv` — this runs for every message and ~64% of them are plumbing
(`Do`, `CheckWindows`, `ClearUI`, `Ask`). The inner action reads state to turn
ids into chips. A narration **must not push messages, must not throw, and must
not assume an entity still exists.**

**Placement** — `placeNarration ref msg entry` is the **only** way a narration
reaches the logger. It decides: hold it, send it, or attach held costs to it.
See §8.2.

---

## 4. Transport and persistence

`ClientLogEntry LogEntry` and `ClientRetractLog Text` on
`Arkham/Classes/GameLogger.hs`. The persist loop is
`Api/Handler/Arkham/Games/Shared.hs` (~line 560–890), with three pending row
shapes: `PendingStructured`, `PendingText` (legacy), `PendingRetract`.

Rows are processed **in order** — a retraction must delete rows written earlier
in the same batch — and `publishLog` is computed **inside the transaction as one
complete value**. §8.6 explains why that matters more than it looks.

Pre-overhaul rows come back as `LogRowLegacy Text (Maybe Int)`
(`Api/Arkham/Helpers.hs:87`), so old games still render.

**Migrations** (all applied): `add_payload_to_log_entries`,
`add_key_to_log_entries`, `log_entry_group` (drops the unique index on `key`,
renames `key` → `group_id`). Note the repo does **not** use sqitch state despite
the `migrations/` layout — see `CLAUDE.md`.

---

## 5. Undo

`logEntryStep` is the undo target, and `step` on the row is load-bearing for
`Undo.hs`. `trimLogFrom gameId toStep` deletes rows at `step >= toStep` — except
chat, which is moved to `toStep - 1` and kept. **Not clamped at zero on
purpose**: clamping to 0 puts the chat *on* step 0 and the `>= toStep` delete
takes it after all. The column is a plain `Int`; a negative step just sorts
first, and only an undo to the very beginning produces one.

Append-only rows are why undo needed no other special handling: there are no
revisions to unwind.

---

## 6. Rendering

| File | Job |
| --- | --- |
| `types/GameLog.ts` | decoders (with `failover`), and `groupLogEntries` turning a flat run into `LogItem[]` |
| `GameLog.vue` | the column: `items`, `autoOpen`, pin/unpin overrides, scrollback `loadOlder`, scroll-position preservation |
| `GameLogEntry.vue` | one entry, recursive; the `Test` band; the annotation rule |
| `LogGroupBlock.vue` | a block: full-width header, members, summary band, stats stripe |
| `LogPart.vue` | parts → DOM; `refName` (card DB lookup, side-letter strip), `refClass` (class colours) |

Two rules worth knowing before you touch them:

- **A block stays open until something outside it follows, and chat does not
  count.** Not "collapse when the summary arrives" — entries keep arriving after
  the result (the clue an investigation discovered lands after the test
  resolves), so closing on the summary shut the block while it was still being
  written to and the result flashed past.
- **An annotation is not nested detail.** An entry whose children are all leaf
  `Notice` rows renders always-visible, no caret, tight under the headline. A
  one-line cost surcharge behind a disclosure triangle, vanishing the moment the
  entry stopped being newest, is not "one entry".

All English lives in `frontend/src/locales/en/log.json` — 98 keys, reviewable in
one file. That is a hard rule, not a convention.

---

## 7. Cost attribution (the most recent work)

`Frozen in Fear` adds an action and nothing said so, so a move that cost two
read as a bug ([#2575]).

The attribution **must be taken at payment and cannot be recovered afterwards.**
The cards that do this use `AdditionalActionCostOf (FirstOneOfPerformed [...])`,
which asks that *none* of its actions has been performed yet
(`Helpers/Action.hs:171`) — and that stops being true the instant the action it
charged for lands in `InvestigatorActionsPerformed`. Ask at `EnterLocation` and
every one of them reports nothing. (This was my first implementation. It
compiled, shipped, and silently produced no output.)

So: `ActiveCost.hs` `getActionCostSurcharges` keeps the source beside each
surcharge (`getActionCostModifier` is now `sum . map snd` of it), and the
`ActionCost` branch pushes `AdditionalCostPaid InvestigatorId Source Cost` — a
**narration-only** message; nothing in the engine reads it. It carries a `Cost`
rather than a count so other costs can use the same seam.

That announcement lands *before* the thing it paid for, so the narrator holds it
and gives it to the next entry: as children normally, or as a block's **first
members** when the next entry opens one (§8.9). `flushNarrator` empties the hold
at the end of every action so nothing is dropped with the ref.

`EnterLocation` now carries `Maybe Movement`, so `moveForced` picks
`log.isMovedTo` over `log.movesTo` — a card dragging an investigator somewhere
no longer reads as a move they chose and paid for.

---

## 8. Invariants that will bite you

Each of these cost real debugging time. They are not stylistic.

**8.1 Rows are append-only. Never revise one.** The first grouping
implementation mutated keyed rows in place and folded children server-side. It
produced three separate bugs — duplicated publishing (three times), a block
frozen on screen, and clobbered undo — all of which became *unrepresentable*
once rows were immutable. If you find yourself wanting to edit a sent row,
you want a new row with a group id instead.

**8.2 `placeNarration` is the only narration path.** `Game.hs` calls
`narrationFor` directly at **two** sites: the main message hook (~6838) and
inside the `Simultaneously` sub-message loop (~6997). There is no single
chokepoint further in. A `narrate` wrapper used to exist and was *never called*
— which is exactly how the cost hold shipped as dead code the first time. It has
been deleted. If you add state to the narrator, both sites must see it.

**8.3 `EnterLocation` only ever arrives inside a `Simultaneously`.** That is why
site 2 exists. `MoveTo` is the *intent* and can still be refused;
`PlaceInvestigator` only fires for vehicles and a few specific cards.

**8.4 Context that must outlive an ask belongs in game state, not the narrator.**
Measured: a skill test spans **four** HTTP actions, because each ask ends one,
and the narrator ref is created per action in `updateGame`. So the ref is the
wrong home for anything that has to span one.

Note what this does *not* mean. Carrying context that is gone by the time an
entry can be built is exactly the problem the state-machine design was for; the
lesson is about *where* to keep it. Game state survives asks (it is serialized
per step), so that is where it goes — `gameCardPlayStack` is the worked example.

An earlier version of this file claimed the cost hold was safe because a cost
and the action it paid for are never separated by an ask. **That was false.**
Research Librarian's `freeReaction` on `AssetEntersPlay #after` ends the action
between paying for the card and resolving it, so the hold flushed and stranded
the payment above a play the log had not written yet. Do not assume this of a
new case — `psql` and compare `step` (§10) will tell you in one query.

**8.5 Emit once.** Several messages fan out to every participant: one skill test
sends `PassedSkillTest_` to the committed skill, the investigator, and each
revealed token. Only the one addressed to `SkillTestInitiatorTarget` is the
result.

**8.6 `publishLog` must be one value computed inside the transaction.** Three
separate duplication bugs all had the same root cause: `publishLog = lastN n
(oldLogEntries <> updatedLog)` where `updatedLog` changed meaning. Only visible
in logs under ~40 rows, because `lastN` truncates the evidence away.

**8.7 Never build an i18n path in Haskell.** `campaignLog.ChasingTheStranger`
was a key I constructed server-side; the real path is
`thePathToCarcosa.key.chasingTheStranger`. Send `LogCampaignKey` and let the
client resolve it with `formatKey`.

**8.8 `log.action.*` keys are transitive phrases.** "moving to", "parleying
with" — they are built for `{verb} {target}`. Reused without a target they cut
off mid-sentence.

**8.9 A group header's children are never drawn.** `LogGroupBlock` renders
`header.body` and nothing else. Attach detail to a header and it vanishes; send
it as a `GroupMember` instead.

**8.10 Do not add a narration *beside* a legacy `send`.** `Campaign/Runner.hs`
already sent `Record`/`RecordCount`, so adding narration produced two lines. The
tell was straight vs. curly quotes. Delete the legacy send in the same change.

**8.11 i18n var names cannot be icon names.** `{action}`, `{skull}`,
`{combat}` are escaped to glyphs before vue-i18n sees them, so the variable is
silently never substituted.

**8.12 Class colour tokens fail as text.** Measured: survivor `#ee4a53` is
3.45:1 on the plain panel and 2.28:1 on the green summary band. `LogPart.vue`
uses lightened literals that clear 4.5:1 against *every* entry background —
check any new background against all of them.

---

## 9. Open work

**Legacy `send` sites — 24 files remain.** Each is safe to delete only once a
narration covers the same message (§8.10). Current list: `Scenario/Runner.hs`,
`Campaign/Runner.hs`, `Investigator/Runner.hs`, `Classes/GameLogger.hs`, plus
individual cards (`RunicAxe`, `LuckyPennyOmenOfMisfortune2`,
`EyeOfTheDjinnVesselOfGoodAndEvil2`, `Geas2`, `TheBeyondBleakNetherworld`,
`ThroughTheGates`, `ViciousAmbush`, `InPursuitOfTheDead`, `BadBlood`,
`Subject5U21`, `CosmicRevelation1`, `VoiceOfRa`, `ThePredatoryHouse`, …).

**Compiled but never seen on screen.** Treat as unverified:

- fight / investigate / parley surcharges landing as a test block's first members
- the full-width group header
- treachery draws, and the Peril split (name it to the drawer, "a Peril card" to others)
- the act/agenda name fallback via `byCardCodeRef`
- scrollback paging end to end
- the skill-test breakdown and adjusted-value band
- investigator class colours

**`surchargeWording` only covers `ActionCost` and `ResourceCost`.** Every other
cost is silent by design — an unexplained extra cost is worse than nothing. Add
wording as cases arise.

**No perf baselines.** Still true from `FINDINGS.md`, and still blocked on a
game export. The measurement list is at the bottom of that file. Take them
before claiming any perf win.

**The frames / state-machine design was never built as described.**
`README.md` describes a stack of frames in the narrator; §8.4 is why that
location does not work. The *idea* is sound and is now realised twice, in the
two places that outlive an action: group ids on the rows themselves (a skill
test, a card play) and `gameCardPlayStack` in game state. Read §8.4 before
reaching for a per-action ref again.

---

## 10. Working on this

- **Do not run `stack build`.** The user builds. Read GHC errors from
  `.claude/build.log` (rolling ~2MB, newest output). A `Message.hs` change
  rebuilds ~8,600 modules — budget 20+ minutes.
- **Verify the module reached the binary.** GHC can report stale content against
  new line numbers; the tell is that the cited line does not contain the named
  construct. Anchor your wait on the log's line count at kickoff, and confirm
  your module appears after the last `Preprocessing library`.
- **`psql` is the fastest way to see what the log actually did.** This is how
  the dead-`narrate` bug was found — two rows at the same `step` proved no ask
  intervened and therefore that the hold had never run:

  ```sql
  select step, seq, payload->>'kind', coalesce(group_id,'-'),
         jsonb_array_length(payload->'children') as kids,
         substring(payload::text from 'log\.[a-zA-Z]+')
  from arkham_log_entries order by id desc limit 12;
  ```

  Local dev DB only: `postgres://localhost:5432/arkham-horror-backend`. **Never
  connect to production.**
- Run `fourmolu` and `hlint` on every Haskell file you touch.
- `-Werror` includes unused-imports, unused-matches and **incomplete-patterns**.
  Removing a helper usually orphans an import.
- Ask before adding specs; the user runs them.

[#2575]: https://github.com/halogenandtoast/ArkhamHorror/issues/2575
