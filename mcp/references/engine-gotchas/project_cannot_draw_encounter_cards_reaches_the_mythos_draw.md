---
name: project_cannot_draw_encounter_cards_reaches_the_mythos_draw
description: CannotDrawCards does NOT stop the mythos encounter draw (it gates offers and the opening hand); CannotDrawEncounterCards is the per-investigator ban, honored in AllDrawEncounterCard and its ForInvestigator
metadata:
  node_type: memory
  type: project
---

`CannotDrawCards` is checked in `handleDrawStartingHand`, the
`StartingHandCardDraw` arm of `handleDoDrawCardsV2`, and the capability/criteria
queries that decide what to *offer*. It is **not** checked on a `DrawCards` the
game pushes, so it never stopped the mythos "each investigator draws an
encounter card" — which is why `docs/homebrew/data/ages-unwound-engine-notes.md`
listed that loop as unreachable by modifiers, and why Ages Unwound VI replaces
the whole draw by hand (`CheckWindows` on the `when AllDrawEncounterCard`
window + `allMatchingDon't`).

`Modifier.CannotDrawEncounterCards` is the per-investigator ban. Honored in two
places, and it needs both:

- `AllDrawEncounterCard` (`Game/Runner.hs`) filters the seat out of the fan-out,
  so it is also absent from **Gloria Goldberg's** draw-order choice (which now
  only routes through her when she is herself in the draw).
- `ForInvestigator iid AllDrawEncounterCard` guards again, because a scenario
  that rebuilds the draw pushes those directly.

What it does not cover: an effect that names an investigator and tells *them* to
draw an encounter card. A seat that must be unreachable by everything still
needs its own enumerated list of bans — see
`Homebrew/AgesUnwound/Locations/RitualCircle_224.hs`, where an out-of-the-game
seat takes `CannotDrawCards`, `CannotDrawEncounterCards`, `CannotGainResources`,
`CannotTakeAction IsAnyAction`, `CannotTriggerAbilityMatching AnyAbility`,
`CannotPlay AnyCard`, `CannotMove`, `CannotBeAttacked`, `CannotBeEngaged`.

Two things that fall out of that list and are easy to get wrong:

- a generic `AnyAbility` ban deliberately does **not** reach forced abilities
  ([[project_cannot_trigger_ability_matching_exemptions]]), so the game's own
  forced text still resolves — which is also what keeps a location's own
  `SilentForcedAbility` able to fire and bring the seat back.
- committing a card to another investigator's test needs no ban: the commit
  check reads `field InvestigatorLocation`, and an `Unplaced` seat has none, so
  both `sameLocation` and `otherLocationOk` fail (`Helpers/SkillTest.hs`).
