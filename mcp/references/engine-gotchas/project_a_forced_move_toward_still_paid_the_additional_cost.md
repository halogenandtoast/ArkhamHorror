---
name: project_a_forced_move_toward_still_paid_the_additional_cost
description: MoveToward/MoveUntil route every step through getCanStepTo, which gates on affording the additional costs (#5894), so a mandatory move was silently cancelled at 0 actions; the forced variants are ForcedMoveToward / getCanForcedStepTo / CanBeMovedCloserToLocation (#5968)
metadata:
  type: project
---

The engine has always known that a forced move waives additional costs — `Movement.hs:80`:

```haskell
-- Forced movement should not require additional costs
forcedMove m = m {moveCancelable = False, movePayAdditionalCosts = False, moveForced = True}
```

`MoveToward` was the one movement path that could not say it. `handleMoveToward`
(`Investigator/Runner/Movement.hs`) hardcoded `moveTo GameSource` — always payable, always
cancellable — and filtered its candidate steps with `getCanStepTo`, whose last conjunct is
`getCanAffordCost iid source [#move] [] (extraCostsToMove <> extraCostsToLeave <> extraCostsToEnter)`.
That filter is right for a voluntary move and is exactly what
[[project_unpayable_enter_cost_was_a_free_move]] added (#5894); on a mandatory move it is the bug.

**The failure is silent in both halves, and the criterion half fires first.** Call of the Sea
(`:return-to-the-innsmouth-conspiracy:060`) is `forced $ TurnEnds #after You` restricted on
`exists (CanMoveCloserToLocation (a.ability 1) You FullyFloodedLocation)`. On Devil Reef, The
Devil of the Depths (07166) puts
`AdditionalCostToEnterMatching (LocationWithTrait Ocean) (ActionCost 2)` on any investigator
standing on an Ocean location outside a vehicle, and the destination (Churning Waters, 07168, an
**Ocean** location that enters play fully flooded) attracts it. The Forced fires at end of turn,
when `remainingActions` is **0**, so `ActionCost 2` is unaffordable: the criterion is false, no
ability is offered, nothing is logged, and the card looks unimplemented (#5968).

`CanMoveCloserToLocation` charges twice over, which is worth knowing when reading `Game.hs`: its
first-step check calls `getCanMoveTo`, which goes `getCanMoveToLocations → getCanStepTo` and
already affords the costs, and *then* it runs its own `getCanAffordCost` on the leave+enter cost.

**The rule, not a card quirk.** Rules Reference, "Additional Costs": *"Additional costs are not
paid when a mandatory instruction … requires an investigator to resolve an effect."* A **Forced**
ability, an act/agenda back and a campaign-guide step are all mandatory instructions, so any of
them that moves an investigator wants the forced path.

| voluntary | mandatory |
| --- | --- |
| `moveToward` / `MoveToward` | `forcedMoveToward` / `ForcedMoveToward` |
| `getCanStepTo` | `getCanForcedStepTo` |
| `getCanMoveTo` | `getCanForcedMoveTo` (uncached) |
| `CanMoveCloserToLocation` | `CanBeMovedCloserToLocation` |

All of these are `getCanStepToWith` / `getCanMoveToLocationsWith` / `canMoveCloserToLocation` under
one `payAdditionalCosts :: Bool`, so the two paths cannot drift. Only the *costs* are waived — a
barricade, `CannotEnterExcept`, `CannotMove` and `canEnterLocation` still gate the forced step.

**The step itself is `mandatoryMove`, NOT `forcedMove`.** `forcedMove` bundles
`moveCancelable = False` with `movePayAdditionalCosts = False`, and `handleMove`'s guard is
`when (canMove || not movement.cancelable)` — so an uncancellable move also walks straight past
`CannotMove`, `CancelMovement` and `CannotMoveExceptByScenarioCardEffects`. The top-of-handler
`CannotMove` check in `handleMoveTowardWith` only sees the state when the `MoveToward` is
processed, not when the step's `Move` resolves. `mandatoryMove` (`Movement.hs`) sets
`movePayAdditionalCosts = False` and `moveForced = True` (the latter only changes the log to "is
moved to") and leaves the move cancelable, which is all the ruling asks for.

**Why a new `HuntMessage` constructor rather than a field.** `HuntMessage` is
`deriveJSON defaultOptions`, so `MoveToward_`'s payload is positional and every queue persisted in
`arkham_steps` would stop decoding if it grew a third argument. `ForcedMoveToward_` is additive, so
old steps are untouched and nothing goes in `legacyHuntMessageTags`. Same reasoning for
`CanBeMovedCloserToLocation` as a sibling of `CanMoveCloserToLocation` rather than a `Bool` on it:
a `Criterion` is serialized inside a stored `Ability`.

`getCanForcedMoveTo` is deliberately **uncached**. The `CacheKey` GADT (`Classes/HasGame.hs:73-75`)
has hand-written `Eq`/`Ord` instances, so telling a paying from a non-paying query apart would mean
a variant of every move key; a forced move is rare enough that the cache is not worth that.

`handleMoveUntil` has the same gap and no card needs it yet — it can take `getCanForcedStepTo` the
day one does.

**Verifying needs no export.** The step query is directly testable:
`getCanStepTo self.id src lid` is False and `getCanForcedStepTo self.id src lid` is True with
`AdditionalCostToEnter (ActionCost 2)` on the destination and `SetActions self.id src 0`. Spec:
`tests/Arkham/Homebrew/ReturnToTheInnsmouthConspiracy/Treacheries/CallOfTheSeaSpec.hs`, modelled on
`tests/Arkham/Asset/Assets/TheGrapevineSpec.hs` (the #5894 sibling).
