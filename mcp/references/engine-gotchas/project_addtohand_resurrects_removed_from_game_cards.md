---
name: project_addtohand_resurrects_removed_from_game_cards
description: AddToHand had no removed-from-game guard, so a delayed "return this to your hand" put a removed card in hand while it stayed in gameRemovedFromPlay (#5688)
metadata:
  type: project
---

`handleAddToHand` (`Arkham/Investigator/Runner/Card.hs`) called `obtainCard` and then appended the
cards to `handL`. `obtainCard` → `ObtainCard` only clears `foundCardsL` / `focusedCardsL`
(`Game/Runner.hs`); it never touches `removedFromPlayL`. So a card in the removed-from-game zone
could be added to a hand **and stay removed** — two zones at once.

Agatha Crane (`c11007`/`c11008`) hit this: her reaction plays a [[Spell]]/[[Insight]] event from the
discard under `RemoveFromGameInsteadOfDiscard`, so `Discarded (EventTarget eid)` routes to
`RemoveFromGame (EventTarget eid)`. But Read the Signs (2) (`c10101`) had already queued
`atEndOfTurn attrs iid $ addToHand iid (only $ toCard attrs)` during `PlayThisEvent`, and that
delayed effect fires unconditionally later.

Fixed by filtering `gameRemovedFromPlay` (via `getRemovedFromPlayCards`, `Helpers/Game.hs`) out of
`handleAddToHand` before `obtainCard`, and pushing `Do (AddToHand iid cards')` with the survivors.

**Why:** it is a family bug, not a Read the Signs quirk. The self-returning events are
`ReadTheSigns2`, `SpectralRazor2`, `Pilfer3` and `BreakingAndEntering2`; the removers are
`AgathaCrane`, `EideticMemory3`, `ThePaintedWorld` and `DeVermisMysteriis2`. Any new pairing would
reopen the hole, so the guard belongs at the `AddToHand` choke point.

**How to apply:** the removed-from-game zone is not globally sealed — `AddToDiscard` deliberately
evicts from `removedFromPlayL` (`Game/Runner.hs`) so a removed card can still be discarded. Only the
hand path is sealed. If a card "comes back from nowhere", diff the export's `removedFromPlay` against
the hand/discard: the same card id in two zones is the signature.
See [[project_deck_ends_only_three_signifiers]].
