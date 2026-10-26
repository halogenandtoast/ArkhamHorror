---
name: project_ability_select_is_empty_without_a_location
description: getAbilitiesMatching is wrapped in guardYourLocation, so select on ANY AbilityMatcher returns [] when the active investigator has no location — which silently voided CannotTriggerAbilityMatching for an Unplaced seat
metadata:
  node_type: memory
  type: project
---

`getAbilitiesMatching` (`Game.hs`, the `Query AbilityMatcher` instance) begins
`guardYourLocation $ \_ -> ...`, and `guardYourLocation` reads
`field InvestigatorLocation` of the **active** investigator and returns `[]` on
`Nothing`. So `select (anyAbilityMatcher)` answers "no abilities exist" for a
seat with no location — `Placement.Unplaced`, which is how a card removes an
investigator from the game.

That made `CannotTriggerAbilityMatching` a no-op exactly where it was needed.
`preventedByInvestigatorModifiers` (`Helpers/Ability.hs`) asked
`elem ability <$> select (AbilityOneOf effectiveMatchers)`; with the pool empty
the answer was always False, so a suspended seat under
`CannotTriggerAbilityMatching AnyAbility` was still offered its fast abilities
in every window. Observed on /Ritual Circle: A Harnessed Future/
(`:ages-unwound:224`): Daisy Walker, `Unplaced` and carrying the full ban list,
was offered The Necronomicon (Petrus de Dacia Translation) (5) ability 3 during
the enemy phase. The modifier was present on her in the game JSON — present and
inert, which is the hard part to spot.

Fixed by matching the one ability instead of sweeping the pool:
`abilityMatches ability (AbilityOneOf effectiveMatchers)`. `abilityMatches` is
the per-ability twin of that `go` function, has no location guard, is exported
through `Arkham/Game.hs-boot` (so `{-# SOURCE #-} Arkham.Game (abilityMatches)`
breaks the cycle), and is cheaper — `AnyAbility` is `pure True` rather than a
sweep over every ability in the game.

**How to apply:** when a modifier's meaning is "this ability is in the set
matching X", reach for `abilityMatches`, not `select` + `elem`. Two independent
failure modes ride on the `select` form: the location guard above, and the
requirement that the ability be `Eq`-present in `getGameAbilities` (a proxied or
modifier-applied ability need not be). More generally, treat an empty result
from a `select` of an `AbilityMatcher` as "unknown" rather than "none" whenever
the active investigator might be out of play. Related:
[[project_cannot_trigger_ability_matching_exemptions]] for why forced abilities
are exempt from a generic matcher in the first place.
