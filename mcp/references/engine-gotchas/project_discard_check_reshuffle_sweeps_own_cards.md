---
name: project_discard_check_reshuffle_sweeps_own_cards
description: "shuffleEncounterDiscardBackIn sweeps the WHOLE discard, so any multi-card discard-and-inspect effect must hold its own cards out or they get shuffled back into the deck mid-effect"
metadata: 
  node_type: memory
  type: project
  originSessionId: 44f18bf6-a615-4074-98a3-02cb4967b577
  modified: 2026-07-30T14:51:34.247Z
---

`shuffleEncounterDiscardBackIn` (`Arkham/Message/Lifted/Card.hs`) pushes a single `ShuffleEncounterDiscardBackIn` that reads the discard **when it executes** and moves all of it into the encounter deck. It has no notion of "cards this effect is currently working with."

That's a trap for any effect that discards N cards and then inspects them, because those cards are already sitting in the discard by the time the deck runs dry. Fortune and Folly's game-icon checks (`ScenarioSpecific "checkGameIcons"` in `Scenario/Scenarios/FortuneAndFolly.hs`) hit it: the printed rule is *"set aside the cards that have already been discarded, shuffle the **remainder** of the discard pile into the encounter deck"*, but the engine shuffled everything. Measured on issue #5303's save (High Roller's Table, all 5 mulliganed, deck at 4): the whole 22-card discard went into the deck, the resulting 5-card hand and the 5 set-aside cards ended up **in the encounter deck**, the discard was left holding 1 card, and a card discarded for the check could be re-drawn into the same hand.

**Why:** the discard pile doubles as the "cards I have discarded so far" scratch space, so a reshuffle silently consumes the effect's own working set.

**How to apply:** when an effect accumulates discarded cards across several messages and can trigger a reshuffle, hold them out explicitly. The pattern that works — `ShuffleEncounterDiscardBackIn` is one message, so filtering the discard synchronously before it and restoring right behind it is exact:

```haskell
push (checkWhen Window.EncounterDeckRunsOutOfCards)
shuffleEncounterDiscardBackIn
scenarioSpecific "restoreGameIconCards" params   -- prepends the held cards back
push msg                                         -- continue the effect
pure $ attrs & discardL %~ filter ((`notElem` heldIds) . (.id))
```

Two details that matter: cards dropped by a mulligan/set-aside step must be tracked in their own record field (they're gone from the working list), and identity comparison must go through the card id — `CardCode`'s `Eq` deliberately cross-matches a/b/c/d side suffixes (see [[project_enemyis_loose_ab_crossmatch]]), so whole-record `notElem` can match the wrong printing.

Adding such a field to a record that rides in the persisted queue needs a hand-written `FromJSON` with `.:? "field" .!= []` — `toResult` (`Arkham/Prelude.hs`) `error`s on a decode failure, so a game parked mid-effect would break on deploy. Verify with `arkham-replay` (see [[project_stale_local_bin_arkham_replay]]).
