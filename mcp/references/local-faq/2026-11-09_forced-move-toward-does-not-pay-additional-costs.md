---
title: A Forced "move toward" does not pay the additional cost to enter
date_added: 2026-11-09
source: Designer ruling via Discord, relayed in issue #5968 (2026-11-09)
affects: [Call of the Sea, The Devil of the Depths, additional cost, movement, Forced]
---

# A Forced "move toward" does not pay the additional cost to enter

Call of the Sea (Return to The Innsmouth Conspiracy, `:return-to-the-innsmouth-conspiracy:060`)
reads **Forced** — after your turn ends, if you are not at a fully flooded location, move toward
the nearest fully flooded location.

On Devil Reef, The Devil of the Depths (07166) is in play: "As an additional cost for you to move
from one [[Ocean]] location to another while not in a vehicle, you must spend 2 actions."

The Forced move **ignores that 2-action cost and happens anyway** — including at end of turn, when
there are no actions left to spend. On Devil Reef that means the investigator is moved to Churning
Waters (07168, which enters play fully flooded).

This is the Rules Reference, "Additional Costs": *"Additional costs are not paid when a mandatory
instruction … requires an investigator to resolve an effect."* A **Forced** ability is such a
mandatory instruction, so the surcharge is neither checked nor charged. The same holds for an
act/agenda back and a campaign-guide step that moves an investigator.

What still stops a Forced move: `CannotMove`, a barricade, `CannotEnterExcept`, and anything that
makes the location unenterable. Only the *costs* are waived.

## Affected cards / systems

- Call of the Sea — file:
  `backend/arkham-api/library/Arkham/Homebrew/ReturnToTheInnsmouthConspiracy/Treacheries/CallOfTheSea.hs`
- The Devil of the Depths — file:
  `backend/arkham-api/library/Arkham/Agenda/Cards/TheInnsmouthConspiracy/DevilReef/TheDevilOfTheDepths.hs`
- Movement: `ForcedMoveToward` / `forcedMoveToward`, `getCanForcedStepTo`,
  `CanBeMovedCloserToLocation`

## Implementation status

verified — the card uses `forcedMoveToward` and restricts on `CanBeMovedCloserToLocation`; both
drop the `getCanAffordCost` gate that `MoveToward` / `CanMoveCloserToLocation` keep, and the step
itself is a `mandatoryMove`, so `movePayAdditionalCosts` is False while the move stays cancelable. Spec:
`backend/arkham-api/tests/Arkham/Homebrew/ReturnToTheInnsmouthConspiracy/Treacheries/CallOfTheSeaSpec.hs`
