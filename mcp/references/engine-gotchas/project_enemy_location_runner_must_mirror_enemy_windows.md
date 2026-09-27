---
name: project_enemy_location_runner_must_mirror_enemy_windows
description: "Enemy-locations get no enemy windows for free — EnemyLocation/Runner must mirror Enemy/Runner's cascade itself (EnemyEvaded fired none, #5581)"
metadata:
  type: project
---

`Arkham.EnemyLocation.Runner` is a *separate* `RunMessage` instance from `Arkham.Enemy.Runner`;
an enemy-location is only surfaced to matchers as an `EnemyLocationEnemyProxy`
(`Arkham/EnemyLocation/EnemyProxy.hs`), which has `runMessage _ p = pure p`. So **every** enemy
message an enemy-location wants must be re-handled in `EnemyLocation/Runner.hs`, including the
*window cascades* the enemy runner opens — the proxy never runs them.

Issue #5581: `EnemyEvaded _ eid | eid == asEnemyId a -> pure $ a & exhaustedL .~ True` exhausted
the enemy-location and stopped. No `EnemyWouldBeEvaded` batch, no when/after `Window.EnemyEvaded`,
so Rita Young ability 1, Dirty Fighting (2), Pickpocketing (2), Dial of Ancients — every
"after you evade an enemy" reaction — were invisible on Hemlock House's living locations. Fixed by
extracting `Evade.pushEvadedWindows` into `Arkham/Behavior/Evade.hs` and calling it from both runners.

Matching itself was never the problem: `Arkham/Game.hs` already folds `toEnemyLocationEnemyProxy`
into the default `select` branch, so `AnyEnemy` / `CanFightEnemyWithOverride` resolve fine *once a
window exists*.

**How to apply:** when a reaction "doesn't fire" on an enemy-location, grep
`EnemyLocation/Runner.hs` for the message before suspecting the matcher — the handler is probably
there but doing only the mechanical half. Put the shared cascade in `Arkham/Behavior/*` (the
modules exist for exactly this) rather than duplicating it. The same gap likely remains for other
enemy cascades not yet exercised on enemy-locations. Related: [[project_enemy_basic_abilities_load_bearing_seam]],
[[project_window_condition_tick_vs_open_tick]].
