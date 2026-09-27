---
name: project_deferred_enemymove_superseded
description: "A move nested inside an outer move's enter-window was clobbered by the outer move's deferred `Do (EnemyMove)`; guard is enemyMovement's destination"
metadata: 
  node_type: memory
  type: project
  originSessionId: aade6093-de2e-40b7-a032-a6e8673e60d7
  modified: 2026-07-25T03:28:50.970Z
---

`EnemyMove` batches `[EnemyEntered, Do (EnemyMove), leaveWindows, afterWindow]`. `EnemyEntered` already applies the placement and then pushes the *entire* enter-window chain, so an arbitrary amount of play can happen before the deferred `Do (EnemyMove)` runs — including a whole fight and a second move of the same enemy. That deferred write then drags the enemy back to the old destination.

Real case (issue #5249): Bat Horror hunter-moved into Southside; Miguel's **Lie in Wait** reaction fought it inside the after-`EnemyEnters` window; Elusive disengaged + moved it to French Hill + exhausted it; then the outer hunter batch resumed and `Do (EnemyMove_ bat Southside)` snapped it back and `EnemyCheckEngagement` re-engaged it.

**Why:** `Do (EnemyMove)` only guarded `leftPlayMidMove` (enemy left play), not "enemy moved somewhere else". And staleness was undetectable because `EnemyMove` reused `enemyMovement` verbatim (`Just _ -> pure enemyMovement`), so a nested move kept the *outer* move's destination.

**How to apply:** `Arkham/Enemy/Runner.hs` now rewrites `moveDestination` to `ToLocation lid` on every `EnemyMove`, and `Do (EnemyMove eid lid)` aborts when `enemyMovement`'s destination is no longer `lid` (`supersededMidMove`). `enemyMovement` is read in only ~4 places, all in that file. When adding another deferred placement write, add the same two guards. Regression test: `tests/Arkham/Enemy/Cards/BatHorrorSpec.hs` (uses Lie in Wait to fight inside the enter window — the only easy way to nest a move in a move in tests).

Related: [[project_placeasset_entity_order]], [[project_window_entry_tick_timing]], [[project_after_enter_engagement_timing]].
