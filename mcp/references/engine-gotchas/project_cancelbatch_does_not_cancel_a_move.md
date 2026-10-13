---
title: project_cancelbatch_does_not_cancel_a_move
description: "CancelBatch on a WouldMove batch cannot stop the move — the move is pushed OUTSIDE the batch. Use cancelMovement (#5843)"
---

`CancelBatch bId` (`Arkham/Game/Runner.hs:2278`) only strips `Would bId _` / `DoBatch bId _`
messages from the queue and empties that batch's `CheckWindows`. It has no reach over
anything pushed outside the batch.

The movement runner splits exactly there. `Arkham/Investigator/Runner/Movement.hs`, the
`Direct` branch:

```haskell
pushBatched batchId
  $ maybeToList mRunWouldMove        -- when WouldMove
  <> maybeToList mRunWhenLeaving     -- when/atIf Leaving
  <> maybeToList mRunAtIfLeaving
  <> [PayAdditionalCost iid batchId leaveCosts]
push (wrapLeave whenCanMoveMsg)      -- WhenCanMove iid innerMsgs -- NOT batched
```

The windows and the leave cost are in the batch; `WhenCanMove`, which carries
`MoveFrom`/`PayAdditionalCost` for the enter cost/`Entering` windows/`MoveTo`, is not. So a
card that cancels the batch from its `WouldMove` ability removes the windows it is standing
in and nothing else — the investigator still moves.

The engine's lever on a move in flight is the `CancelMovement MovementId` modifier, checked
in `handleWhenCanMove` and `handleResolveMovement` (`Movement.hs:415`, `:472`) and applied
by `cancelMovement` (`Arkham/Message/Lifted.hs:3488`), which falls back to a `CannotMove`
modifier when there is no movement in the slot yet.

```haskell
FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
  assignDamage iid (attrs.ability 1) 1
  cancelMovement (attrs.ability 1) iid
  pure l
```

Cancelling this way lets the leaving windows fire and then stops at `WhenCanMove`, so the
enter cost is never paid — correct for "cancel the effects of the move".

Every would-move-test card uses `cancelMovement`: ArkhamWoodsTwistingPaths, TitanicRamp
182–185, SnakePit, SkaiRiver, GrandBazaarBusyWalkway, WeedChokedBeach, HedgeMaze, and
`summitEntryFailed` in `Scenarios/TheDrownedCity/ObsidianCanyons/Helpers.hs`.

Bug this fixed (#5843): Narrow Shaft (The Pallid Mask) was the lone holdout, reading the
batch id off `getWindowStack` at `FailedThisSkillTest` time and pushing `CancelBatch`.
Failing the agility (3) test dealt the damage and moved the investigator anyway, revealing
the destination and spending the Catacombs clue toll.

`CancelBatch` is still right for batches whose payload *is* batched — the would-be-defeated
batch (TidalMemento, Maria Rivera, Old Memory) and the scenario-event batches Milkhouse
cancels. Read the producer before assuming.
