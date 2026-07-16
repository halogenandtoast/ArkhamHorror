# Multi-Seat Barrier Redesign

Status: **proposed** (design approved, Phase 1 in progress)
Owner: backend-systems-engineer · Specs: qa-test-engineer · Gate: tech-lead-reviewer

## Problem

"N players decide simultaneously, then the game continues once all are ready"
is used for deck selection, deck upgrades, story reads, and fast-player
windows. None of these is implemented as a real barrier. They are all crammed
into a single self-mutating `AskMap :: Map PlayerId Question` message and
disambiguated at runtime by fragile heuristics.

### How it works today

`chooseDecks` (`Arkham/Message.hs`) pushes:

```
Run [ SetGameState (IsChooseDecks pids)
    , ChoosingDecks                       -- wipes investigators, GameState := IsChooseDecks
    , AskMap {pid -> ChooseDeck}          -- parks; publishes gameQuestion
    , DoneChoosingDecks                    -- GameState := IsActive
    ]
```

* **The barrier is rebuilt on every answer.** Each answer deletes its own seat
  and re-pushes `AskMap question'` (`Entity/Answer.hs` ~337, ~506).
* **Release is queue-position, not state.** "Everyone done" == "no more AskMap
  re-pushed, so the pop reaches `DoneChoosingDecks`." The `[PlayerId]` inside
  `IsChooseDecks` is never consulted.
* **Per-seat follow-ups are folded into the shared map.** A deck with
  trauma / boon / Morrígan / Eldritch Brand pushes an interactive `Ask` during
  `InitDeck`; `runMessages` folds it into the AskMap
  (`Arkham/Game.hs` ~6493-6505) only while *another* `ChooseDeck` is still
  parked. The seat's non-interactive tail (`DoStep 1`, XP, `LoadSideDeck`) is
  left at the queue front, decoupled from the question that gated it.

### Failure classes this produces

* **#5173** — the decoupled InitDeck tail + `DoneChoosingDecks` + the prologue
  `CampaignStep` leak past scenario setup, firing `Ask ContinueCampaign`
  mid-round → soft-lock. Trigger: In the Thick of It (`PurchaseAnyTrauma 2`) in
  a multiplayer deck.
* **Last-seat race** — the fold needs another `ChooseDeck` still parked
  (`moreChooseDecks`). The last player to pick gets no fold; their follow-up
  races `DoneChoosingDecks`.
* **Lost-continuation** — the release lives only in the queue; if lost the game
  sticks in `IsChooseDecks` forever → self-heal hack (`Arkham/Game.hs`
  ~6382-6391).
* **Stale-seat re-ask (#5159 / #5164 / #5160)** — the generic answer path must
  special-case `Map.filter isDeckQuestion` and `isRegeneratedWindowChoose`
  (`Entity/Answer.hs` ~506, ~514) so deck-barrier semantics don't corrupt
  story reads and fast windows.
* **Morrígan dodge** (`Arkham/Campaign/Runner.hs` ~242) —
  `insertAfterMatchingOrNow … (== DoneChoosingDecks)` is a manual workaround of
  the same fold hazard.

Root smell: an implicit barrier expressed as a mutating message + fold rules +
queue-position invariants, instead of a first-class primitive with
self-contained per-seat sub-flows and a state-driven join.

## Design

One primitive with an explicit **join policy** — the taxonomy that already
exists implicitly, made declarative.

| Policy | Meaning | Replaces | Used by |
|---|---|---|---|
| `JoinAll` | continuation runs only when **every** seat's sub-flow completes | AskMap re-push + queued `DoneChoosingDecks` + self-heal | ChooseDeck, ChooseUpgradeDeck |
| `JoinAny` | first responder decides for the table; other slots dropped | Read continuation + `isDeckQuestion` filter | story `Read` (lead decides) |
| `JoinIndependent` | no join; each seat regenerates its own slot | `isRegeneratedWindowChoose` | PlayerWindow / fast windows |

### State model — pending set + continuation live in state, not the queue

```haskell
data JoinPolicy = JoinAll | JoinAny | JoinIndependent

data SimultaneousAsk msg = SimultaneousAsk
  { saPolicy       :: JoinPolicy
  , saSlots        :: Map PlayerId (Question msg)  -- seats still owing an answer
  , saDeferred     :: [msg]   -- interactive setup, resolved after the join
  , saContinuation :: [msg]   -- run when the join condition is met
  }

-- saPending = Map.keysSet . saSlots
-- JoinAll releases iff  null saSlots

-- in Game state:
-- gameSimultaneousAsks :: Map BatchId (SimultaneousAsk Message)
```

`gameQuestion :: Map PlayerId (Question Message)` stays the published surface
(frontend keeps reading `gameQuestion[myPid]`). It becomes authoritative
per-seat slots that mutate independently, not a map rebuilt on every answer.

> **Correction (implementation, Phase 1).** An earlier draft specified
> `saContinuation :: BatchId`, "reusing the existing `DoBatch` infrastructure so
> the continuation is durable state." **That is not implementable.** The batch
> infra is *entirely queue-based*: `Would BatchId [Message]` carries its
> messages inline, and `CancelBatch` / `IgnoreBatch`
> (`Arkham/Game/Runner.hs` ~1955-1972) implement themselves by scanning and
> filtering the queue. There is no durable batch store to point a `BatchId` at —
> the only batch state on `Game` is `gameCurrentBatchId :: Maybe BatchId`.
> Pointing the continuation at a `BatchId` would put it right back in the queue,
> which is the bug. The barrier therefore **stores `[Message]` directly** and
> reuses `BatchId` only as the barrier's identity/key.

### Lifecycle

1. `BeginSimultaneousAsk bid policy slots continuation` — seeds
   `gameSimultaneousAsks` and parks every seat's slot into `gameQuestion`.
2. Seat answers → clear only that seat's slot → `SeatResolved bid pid`.
3. `SeatResolved bid pid` → drop the seat from `saSlots` → evaluate policy:
   * `JoinAll` → fire `saDeferred` then `saContinuation` when `saSlots` is empty
   * `JoinAny` → fire on first resolution, clear remaining slots
   * `JoinIndependent` → no continuation

### Deck setup as an instance

`BeginSimultaneousAsk bid JoinAll {seat -> ChooseDeck} [DoneChoosingDecks, …]`.
AI seats never enter `saSlots`; their decklists load in place before the ask
begins.

> **Correction (implementation, Phase 1).** An earlier draft specified that each
> seat's interactive setup (trauma, Morrígan, Eldritch Brand) runs *inside* a
> self-contained per-seat sub-flow, concurrently with other seats. **This was
> traced and reproduces #5173 — it is unsound on this engine.** The message
> queue is **global**. If seat A parks mid-sub-flow, A's remaining tail stays in
> that global queue; when seat B answers *anything*, the queue resumes draining
> and drains **A's** tail — firing `SeatResolved A` and then the continuation
> while A is still parked. Genuinely concurrent per-seat sub-flows would require
> per-seat queues, which the engine does not have.
>
> Instead, Phase 1 generalizes the pattern the codebase had already discovered
> ad hoc in the Morrígan dodge: **all interactive setup defers past the barrier**
> via `saDeferred` and resolves one seat at a time after the join.
> `DoStep 1` (Spiritual Healing) reads purchased trauma, so a seat's tail defers
> as a unit. The invariant, documented on the type:
> **a barrier seat's sub-flow never parks.**
>
> Consequence for the product goal: **deck choice is concurrent; trauma / boon /
> Morrígan / Eldritch Brand are sequential after the join.** Full per-seat
> concurrency for those prompts is out of reach without per-seat queues — see
> Open Questions.

Because nothing folds into a shared structure and the continuation only fires
from the state transition, the InitDeck tail can no longer leak past the
barrier — #5173 becomes structurally impossible.

### Removed after migration

* Morrígan `insertAfterMatchingOrNow` dodge — **removed in Phase 1**
  (generalized into `saDeferred`, in both `Campaign/Runner.hs` and
  `Scenario/Runner.hs`)
* `moreChooseDecks` fold — `Arkham/Game.hs` ~6493-6505 — **retained**, still
  load-bearing for `ChooseUpgradeDeck`; remove in Phase 2
* self-heal hack — `Arkham/Game.hs` ~6382-6391 — **retained**, still reachable
  via `ChooseUpgradeDeck` *and* The Dream Eaters' `PrologueStepPart 11/12`,
  which hand-roll `ChoosingDecks` + sequential `Ask pid ChooseDeck` +
  `DoneChoosingDecks` rather than calling `chooseDecks` (this doc originally
  missed those call sites). Phase 1 guards it with
  `null (gameSimultaneousAsks g)` so it cannot pre-empt a live barrier.
* `isDeckQuestion` filter — `Entity/Answer.hs` ~506 — Phase 3
* `isRegeneratedWindowChoose` — `Entity/Answer.hs` ~514 — Phase 4
* per-answer `AskMap question'` rebuild — `Entity/Answer.hs` ~337-345 — deck
  path migrated in Phase 1; `Api/Handler/Arkham/Decks.hs` ~138 holds a third
  copy for the upgrade endpoint, left for Phase 2

## Rollout

Each phase is shippable and specced independently.

1. **Primitive + `JoinAll`, migrate ChooseDeck only.** Fixes #5173.
   Specs: 2p In the Thick of It, Morrígan boon, mixed human/AI.
2. **Migrate ChooseUpgradeDeck** to `JoinAll`. Spec: upgrade window between two
   scenarios with an interactive upgrade choice.
3. **`JoinAny`, migrate story `Read`.** Regression specs: #5159, #5164.
4. **`JoinIndependent`, migrate fast windows.** Regression: #5160. Delete the
   old `AskMap` sniffing paths.

## Constraints

* Frontend contract (`gameQuestion[myPid]`) is preserved throughout — pure
  backend migration.
* Do **not** run `stack build`; the user builds and reports errors.
* Blast radius is every campaign start, upgrade window, story read, and fast
  window. Land behind multiplayer specs; `tech-lead-reviewer` gates each phase.

## Per-seat concurrency — evaluated and deferred

The stated product goal is that each player independently resolves deck /
trauma / boon / Morrígan without holding up the others. Phase 1 delivers that
for **deck choice only**; the interactive prompts are sequential after the join,
because the global queue makes concurrent parked sub-flows unsound (a seat's
parked tail sits in the shared queue and any other seat's answer drains it —
the #5173 mechanism).

Three mechanisms were evaluated to make the interactive prompts concurrent:

* **Park-time extraction** — lift a parked seat's remaining sub-flow out of the
  global queue into its barrier slot. Buildable and correct for today's
  deck-init tail, but soundness is *contingent, not structural*: it holds only
  because the tail is tame (linear single-`Ask` parks, prepending pushes, no
  windows, no mid-setup defeat), none of which is type-enforced. Would ship
  with a loud extraction-time assertion. Failure mode: **loud** (`error` at
  extraction).
* **Per-seat queues** — each seat's sub-flow runs against its own `newQueue`.
  Initially looked structurally clean, but inspection found it does **not**
  clear the "no fragility tax" bar — it relocates and *enlarges* the tax:
  1. no bounded sub-flow drainer exists; reusing the top-level `runMessages`
     loop no-ops only by accident of which phase deck-selection runs in
     (`Arkham/Game.hs` ~6388-6436) — a clean version needs a **new bounded-drain
     primitive on the hot loop**;
  2. each park's `putGame` writes a single-seat `gameQuestion`
     (`toExternalGame` at `Arkham/Game.hs:386` sets `gameQuestion = mq`, and the
     `Ask` branch passes `singletonMap pid q` at ~6536), so concurrent parks
     clobber each other's published slots — a **merge/republish layer** over the
     question surface is required;
  3. a shared effect trapped in a private queue fails **silently**;
  4. touches the **undo/replay** action-diff boundary, untested against a nested
     `runMessages` mutating the game mid-request.
* **Ship sequential** — keep the interactive prompts sequential after the join.

**Decision: ship sequential.** Rationale: for #5173's actual trigger (In the
Thick of It — one copy in one deck → one prompt) concurrency buys **nothing**;
it only helps when multiple seats have interactive deck-init *simultaneously*
(several In-the-Thick-of-It, or Morrígan across decks), which is uncommon. Per-
seat queues are the worst mechanism for that marginal gain (largest diff, silent
failure, run-loop + undo surface). Phase 1's `saDeferred` / `saSlots` shape is
retained unchanged. If concurrency is ever genuinely wanted, park-time
extraction + a loud guard is the mechanism to reach for, not per-seat queues.

## Open questions

* **Prompt-timing behavior change.** On the Dream Eaters / sequential and
  standalone paths, trauma / Eldritch Brand / XP now resolve **after**
  `DoneChoosingDecks` (previously only Morrígan did). Same class of fix, but it
  moves *when* those prompts appear to the player. Needs a product call.

## Verification note (#5173)

The bug originates in the `ChooseDecks` window, which predates the earliest
retained replay step in the reported export, so it cannot be reproduced via
`arkham-replay --undo` on that file. Verify with a fresh multiplayer spec (a
deck containing In the Thick of It) asserting the campaign reaches the scenario
with no stranded `ContinueCampaign` question.
