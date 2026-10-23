---
title: project_unpayable_enter_cost_was_a_free_move
description: "An unpayable AdditionalCostToEnter COMPLETED the move for free — ActiveCost falls back to IgnoreBatch, which cannot reach the enter cost; and MoveUntil/MoveToward never checked affordability (#5894)"
---

Two separate holes on the path a non-action move takes.

## 1. `MoveUntil` / `MoveToward` offered steps the investigator could not enter

`handleMoveUntil` / `handleMoveToward` (`Arkham/Investigator/Runner/Movement.hs`) picked
the next step straight off the geometry:

```haskell
closestLocationIds <- select $ ClosestPathLocation loc lid
when (notNull closestLocationIds) do
  Choose.chooseTargetM investigatorId closestLocationIds \nextLid -> do
    moveTo GameSource investigatorId nextLid
    push $ MoveUntil lid target
```

The move **action** never has this problem: its destinations go through
`getCanMoveToLocations_` (`Arkham/Helpers/Location.hs`), which runs `getCanAffordCost`
over the additional costs to move / leave / enter, and its own ability cost assembly in
`getCanAffordAbilityCost` (`Arkham/Helpers/Ability.hs`) folds
`enterCosts <> leaveCosts <> moveCosts` in with `ActionCost 1`. `ClosestPathLocation` is
geometry, not permission — anything that moves an investigator without going through the
move ability has to filter its own destinations.

## 2. An unpayable additional enter cost completed the move for free

`ActiveCost.hs`'s `PayCost` handler, when `canStillAfford` is False and the target is
`ForAdditionalCost batchId`, falls back to:

```haskell
ForAdditionalCost batchId -> do
  push $ IgnoreBatch batchId
  pure c
```

But `Direct` puts the enter cost **and everything after it** in a plain list:

```haskell
innerMsgs =
  [MoveFrom source iid from | …]
    <> [PayAdditionalCost iid batchId enterCosts, runWhenEntering, runAtIfEntering]
    <> postEnterMsgs            -- ResolveMovement, MoveTo, the Moves windows
whenCanMoveMsg = WhenCanMove iid innerMsgs
moveBatch = Would batchId (wouldMove/leaving windows <> [PayAdditionalCost … leaveCosts])
```

`IgnoreBatch bId` (`Arkham/Game/Runner.hs`) removes only `Would bId _` / `DoBatch bId _`.
The batch holds the would-move/leaving windows and the *leave* cost; the enter cost and the
move itself are outside it. So the fallback cancelled nothing that mattered and the
investigator entered unpaid. Same shape as
[[project_cancelbatch_does_not_cancel_a_move]]. `CannotMoveTo` is an unpayable
`AdditionalCostToEnterMatching` (`Helpers/Modifiers.hs`), so it leaked the same way.

The `--trace` signature:

```
> WhenCanMove "60351" [MoveFrom …, PayAdditionalCost … (GroupClueCost (PerPlayer 3) Anywhere), …, ResolveMovement "60351", …]
> PayCost … (GroupClueCost (PerPlayer 3) Anywhere)
> IgnoreBatch 6a71a4fd-…
> Simultaneously [… EnterLocation "60351" 77cbd0d9-… …]
```

## The fix

`getCanStepTo iid source lid` in `Arkham/Helpers/Location.hs` — the per-destination half
of `getCanMoveToLocations_` (barricades, `CannotEnterExcept`, and affordability of the
move/leave/enter costs). `getCanMoveToLocations_` now calls it, and both path-step
handlers filter their candidates through it. Plus a
`getCanAffordCost iid source [#move] [] enterCosts` guard in `Direct` that skips the final
pushes outright, so no caller of `Move` can turn an unpayable enter cost into a free move.

`MoveUntil` / `MoveToward` carry no source, so `getCanStepTo` is called with `GameSource`.
The whole-mover checks (`CannotMoveExceptByScenarioCardEffects`, vehicles) deliberately
stay in `getCanMoveToLocations_`: applying them with `GameSource` would wrongly block a
genuine scenario effect such as
`Homebrew/ReturnToTheInnsmouthConspiracy/Treacheries/CallOfTheSea.hs`.

**Found by** #5894: The Grapevine (2) ("Move, one location at a time, along the shortest
path to that enemy's location and engage it") walked into Queen of Ash's unrevealed
**Underground Cistern** — `AdditionalCostToEnter (GroupClueCost (PerPlayer 3) Anywhere)`,
12 clues at 4 players, group held 7 — and flipped Search the Sewers. Both shortest paths
were 2 steps, so the Cistern was offered alongside the Sewer Culvert.
