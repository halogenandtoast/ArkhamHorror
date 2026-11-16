---
name: project_spawn_enemyentered_clobbers_mid_spawn_move
description: "A spawn's deferred EnemyEntered dragged an Elusive-fled enemy back to its spawn location; guard with isNothing spawnDetails + moveDestination /= lid"
metadata:
  node_type: memory
  type: project
---

`Do (EnemySpawn)` pushes `Will (EnemyEngageInvestigator)` **before**
`EnemyEntered` (deliberate, Doppelgänger 11058), so the whole engage window
resolves while the spawn's placement write is still queued. Anything that moves
the enemy in that window is then undone by `EnemyEntered`'s
`placementL .~ AtLocation lid`.

Real case (#6007): Voices in the Night (13116, `forced $ EnemyEngaged #after You
AnyEnemy`) made a just-spawned Bat Horror attack; the attack's **Elusive**
disengaged + moved + exhausted it, then the spawn's `EnemyEntered bat
sideChamber1` dragged it back onto the investigator. Tell in the export: the
enemy's `movement.moveDestination` still points at the fled-to location while
`placement` is the spawn location, and `spawnDetails` is `null`.

**How to apply:** `EnemyEntered` now skips the placement write when
`isNothing enemySpawnDetails && maybe False ((/= ToLocation lid) . moveDestination) enemyMovement`.
`EnemyMove` clears `spawnDetailsL` and rewrites `moveDestination` on every move,
so that pair means "something moved this enemy out from under its spawn".
Do NOT use a bare `supersededMidMove` here: `Do (EngageEnemy)` and the spawn
branches push `EnemyEntered` without maintaining `enemyMovement`, so stale
movement would abort legitimate placements. Sibling guard:
[[project_deferred_enemymove_superseded]] (#5249), which only ever covered the
move path.

Also: `stack exec arkham-replay` resolves a **stale** copy under
`.stack-work/install/...`; `make api.watch` only refreshes
`arkham-api/.stack-work/dist/<arch>/<ghc>/build/arkham-replay/arkham-replay`.
Run that path directly or verification gives a false negative.
