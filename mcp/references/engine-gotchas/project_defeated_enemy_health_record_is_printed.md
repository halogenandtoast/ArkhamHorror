---
name: project_defeated_enemy_health_record_is_printed
description: "Two records of a defeated enemy's health with opposite meanings — ScenarioDefeatedEnemies must be PRINTED (Bounty/Ancestral Token/Autopsy Report 3/Twisting Catwalks), turn history stays MODIFIED (\"Let God sort them out...\") (#5689)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 3b8fbb65-40e9-42aa-9434-c2c6bb845c86
  modified: 2026-09-11T11:39:39.714Z
---

A defeated enemy's health is recorded in **two** places, and they must hold different numbers:

- `ScenarioDefeatedEnemies` (`Scenario/Runner.hs`, `Do (X.Defeated (EnemyTarget eid) …)`) —
  read **only** by `getDefeatedEnemyHealth` (`Helpers/Enemy.hs`). Every caller's card text says
  *printed* health: Bounty, Ancestral Token, Autopsy Report (3), Twisting Catwalks. Must record
  `calculatePrinted (enemyHealth eattrs)`.
- `HistoryEnemiesDefeated` (`Game/Runner.hs`, `Helpers.Message.Defeated (EnemyTarget eid)`) —
  read by `DefeatedEnemiesWithTotalHealth` for `"Let God sort them out..."` (`06160`), whose
  text is a bare "a total of 6 or more health" with no *printed* qualifier. Stays on
  `EnemyHealth` (modified).

Both write the same `DefeatedEnemyAttrs` record type, so the shared field name
`defeatedEnemyHealth` hides the divergence. The scenario one was writing
`fieldWithDefault printedHealth EnemyHealth eid` — modified — so any enemy with a
`HealthModifier` paid out the wrong X, silently.

The helper's two branches also have to agree: the live branch reads `EnemyHealthActual`, which
is the raw printed calculation; only `EnemyHealth` folds in `HealthModifier` /
`HealthModifierWithMin` (`Game.hs`, the `EnemyHealth` arm). The fallback branch was the odd one
out, and it is the branch that actually fires — `clearRemovedEntities` drops the entity before
most `IfEnemyDefeated` reactions resolve (see [[project_after_dealt_damage_is_post_defeat]]).

**Why:** the bug is invisible in a normal game — printed and modified agree unless something is
buffing enemy health, and there is no log line either way.

**How to apply:** before using `getDefeatedEnemyHealth`, confirm the card says *printed*; if a
card ever needs the modified value, add a second helper rather than changing what the scenario
map stores. Test it with a printed value plus a **negative** `HealthModifier`, so one hit still
defeats the enemy and the two numbers differ (`tests/Arkham/Helpers/EnemySpec.hs`).
