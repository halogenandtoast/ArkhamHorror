---
name: project_discardedfromhand_is_per_card_not_per_event
description: "DiscardedFromHand opens one window PER CARD, so a 'discard N cards' batch fires responders N times; DiscardedFromHandBatch is the once-per-event window (#5548)"
metadata: 
  node_type: memory
  type: project
  originSessionId: c396a736-66c8-4a73-a18e-40eb5c2c083c
  modified: 2026-08-29T08:44:21.375Z
---

`handleDiscardCard` (`Arkham/Investigator/Runner/Card.hs`) opens a **per-card**
`Window.DiscardedFromHand iid source card` pair for every `DiscardCard` message. A
`DiscardFromHand` batch (`handleDoDiscardFromHand` → `chooseN` / `chooseOneAtATime`)
resolves as N separate `DiscardCard`s, each with its own `CheckWindows`, so a forced or
triggered responder fires **N times**. That is wrong for any card worded "discard **1 or
more** cards from hand" — Miasmatic Shadow (`10724`) attacked once per card when trimming
your hand at the end of a Hemlock Vale prelude (#5548).

**The once-per-event window is `Window.DiscardedFromHandBatch InvestigatorId Source [Card]`**
(matcher `DiscardedFromHandBatch Timing Who SourceMatcher`, no card matcher), modelled on
`DiscardedTopOfEncounterDeckBatch`. Two emission points cover every hand discard exactly once:

- `handleDoneDiscarding` — `DoneDiscarding` fires **exactly once per batch and always after
  every card in it** (the `chooseN` re-ask sits in front of it in the queue), so it is the
  reliable batch-end marker. Cards are accumulated into `HandDiscard.discardBatchCards` by
  `handleDoDiscardCard` (same `updateHandDiscard` that decrements `discardAmount`).
- `handleDiscardCard` when `investigatorDiscarding` is `Nothing` — a bare `DiscardCard`
  (Lt. Wilson Stewart, Obsessive, `ActiveCost`'s `DiscardRandomCardCost`) never reaches
  `DoneDiscarding`, so it opens its own single-card batch window.

Most hand discards *do* route through `DiscardFromHand`: `discardCard` /
`chooseAndDiscardCard` in `Helpers/Message/Discard.hs` build a `HandDiscard` with a
`CardWithId` filter, so cost-driven discards are batches of one.

**Do not "fix" this by merging the N per-card payloads into one `CheckWindows`.** It would
work — `getActions` uses `anyM` over the window list and `runWindow` pushes one `UseAbility`
per ability, so an ability matching any payload is offered once — but it regresses every
per-card listener: George Barnaby reads `cardDiscarded` (first payload only) and would lose
the choice of which discarded card he keeps. Keep the per-card windows; add to them.

`HandDiscard` has a **hand-written lenient `FromJSON`**, so new fields take
`.:? "field" .!= default` and in-flight saved queues keep parsing. Note the field is
`discardBatchCards`, not `discardedCards` — `Arkham.Cost.discardedCards :: Payment -> [Card]`
already exists and is view-patterned all over the card modules.

Diagnosing: `arkham-replay --undo N --answers … --trace`, then count `InitiateEnemyAttack_`
against `Do (DiscardCard` in the trace. For #5548, `--undo 6` landed exactly on the
`ChooseN {amount = 3}` hand-trim prompt: 3 discards → 3 attacks before, 1 after.

Related: [[project_discardedfromhand_when_window]],
[[project_reentrant_discard_laps_rescue_moves]], [[project_yourlocation_window_handler_fanout]].
