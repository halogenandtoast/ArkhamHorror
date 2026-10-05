# Game log overhaul — journal

**Read this first.** Newest entry at the top. An agent picking this up should be
able to start from the "Next up" line without re-deriving anything.

**Status: Phase 0 complete (audit). All design decisions taken, nesting
settled on "live tail". Phase 1 not started.**

**Next up:** Phase 1 step 1 — `Arkham.Log.Entry` + serialization, pure
addition, behaviour-neutral. Then the transport, then the renderer. Baselines
(bottom of `FINDINGS.md`) are still unmeasured and blocked on a game export;
take them before Phase 2.

---

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
