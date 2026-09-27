---
name: disableeffect-reran-ondisable-for-removed-effect
description: DisableEffect re-ran a removed effect's onDisable body because maybeEffect falls back to gameActionRemovedEntities; one duration ends via several messages
metadata:
  type: project
---

`Game/Runner.hs`'s `DisableEffect` used `maybeEffect`, which falls back to
`getRemovedEntity effectsL` (`Game/Utils.hs`). A single duration is ended by
**several** messages — `EffectMoveWindow` is disabled by `MoveAction _ _ _ False`,
by `Move _`, **and** by `ResolvedMovement` — and each pushes its own
`DisableEffect`. The first ran the body and parked a `finished = True` copy in
`gameActionRemovedEntities`; the later ones still found that copy and ran
`effectOnDisable` again.

`Arkham.Effect`'s `RunMessage` guard on `effectFinished` does not help: the
duplicate `DisableEffect` messages were already queued, and the re-run happens in
the Game runner, not in the effect entity.

Everything built on `afterMove` (`Message/Lifted.hs`: `effect iid $ removeOn #move
>> onDisable body`) therefore ran its body up to 3 times. It went unnoticed because
the existing caller (Return to Curtain Call's The Stranger — `setActions 0` +
`endYourTurn`) is idempotent. Circus Ex Mortis' Close Watch spawned an enemy per
trigger.

Fix: `DisableEffect` looks up the **live** effect only
(`preview (entitiesL . effectsL . ix effectId) g`), so a repeat is a full no-op.

Related: [[project_in_discard_ability_outlives_the_discard]] — the same
`gameActionRemovedEntities` fallback biting a treachery.
