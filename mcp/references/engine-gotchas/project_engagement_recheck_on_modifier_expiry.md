---
name: project_engagement_recheck_on_modifier_expiry
description: "Enemies only re-check engagement at BeginRoundWindow and After (EndTurn _) — an expiring CannotBeEngaged gets a CheckEnemyEngagement rider attached at effect creation (Grisly \"Mask\", #5587)"
metadata:
  type: project
---

The RR says "**any time** a ready unengaged enemy is at the same location as an investigator, it
engages that investigator" — a constant state check. The engine approximates it with discrete
`EnemyCheckEngagement` pushes, and there are only two periodic ones
(`Arkham/Enemy/Runner.hs`): `After (EndTurn _)` and `BeginRoundWindow`. Everything else is an
event-driven push (spawn, move, `PlaceInvestigator`, `EnemyEntered`, ready, aloof loss).

So a modifier that makes an investigator unengageable and **expires mid-round** leaves enemies
detached until the *end* of that turn. Issue #5587, Grisly "Mask" (11582): `CannotBeEngaged` runs
`until the beginning of your next turn`; `BeginRoundWindow` fires while it is still live, `BeginTurn`
disables it, and the next check is `After (EndTurn _)` — so the player spent their whole turn able to
move away from and attack an enemy that was still unengaged.

Fixed generically at **effect creation**, not in the runner: `withEngagementRecheck`
(`Arkham/Effect.hs`) inspects the freshly built `Effect` and, when its metadata is `EffectModifiers`
on an `InvestigatorTarget` carrying `CannotBeEngaged` / `CannotBeEngagedBy _`, prepends
`CheckEnemyEngagement iid` to `effectOnDisable`. It wraps both creation entry points that can carry
investigator modifier metadata — `createEffect` (the `Arkham/Effect/Builder.hs` DSL → `GenericEffect`)
and `createWindowModifierEffect` (`roundModifier` / `turnModifier` / `nextTurnModifiers` / …).

`DisableEffect` (`Arkham/Game/Runner.hs`) then drains `effectOnDisable` as it already did, *after*
the effect leaves `entitiesL . effectsL`, so modifiers are recomputed by the time the check runs.
Keeping the policy in `Effect.hs` keeps the game runner free of card-shaped rules, and the rider is
serialized with the effect (`onDisable`), so it survives save/load.

**Do not** try to do this from the card by handling `BeginTurn` yourself. Entities fan out in a fixed
order (`Arkham/Entities.hs` — effects **before** assets) and `pushAll msgs = msgs <> queue` prepends,
so an asset's push lands *in front of* the effect's own `DisableEffect` and the check runs while the
modifier is still active. The card-local escape hatch, if you ever need one, is
`EffectAttrs.effectOnDisable` via the builder DSL (`Arkham/Effect/Builder.hs`: `effectWithSource` /
`during` / `apply` / `onDisable`) — same drain point, correct ordering; `afterMove`
(`Arkham/Message/Lifted.hs`) is the precedent.

**How to apply:** any "the modifier wore off but the board didn't react" bug belongs in
`withEngagementRecheck`'s neighbourhood — attach the reaction to the effect at creation, not in the
card and not in `Game/Runner.hs`. Related: [[project_window_condition_tick_vs_open_tick]],
[[project_enemy_location_runner_must_mirror_enemy_windows]].
