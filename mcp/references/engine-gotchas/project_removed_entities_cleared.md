---
title: project_removed_entities_cleared
description: "Removed enemies/assets keep their entity briefly then are GC'd by clearRemovedEntities, so cross-boundary readers must use the defeatedEnemies record, not a field projection"
---

`RemoveEnemy` does **not** delete the entity — it sets `enemyPlacement = OutOfPlay RemovedZone` and leaves it in `entitiesL`, so effects resolving alongside the defeat can still read its fields. `clearRemovedEntities` (`Entities.hs`) later drops it, called from exactly two places in `Game/Runner.hs`:

- `ResolvedAbility` — when the last active ability finishes (`null remainingAbilities`)
- `BeginTurn`

So an enemy defeated inside an ability is queryable during that ability and **gone afterwards**. An `IfEnemyDefeated #after` window answered past that boundary sees no enemy at all, and a field projection returns `Nothing` — silently, no crash. This is deliberate GC, not a leak; serialization does nothing here (`instance ToJSON Entities` is a plain `genericToJSON` with no filtering — do not confuse the `Map.filter` inside `clearRemovedEntities` for a ToJSON filter).

The engine already records what late readers need, at defeat time:
- `scenarioDefeatedEnemies :: Map EnemyId DefeatedEnemyAttrs` (written by `Do (Defeated (EnemyTarget eid))` in `Scenario/Runner.hs`), carrying `defeatedEnemyHealth` = **modified** health.
- Per-investigator `HistoryEnemiesDefeated`.
- `getEnemiesMatching (DefeatedEnemy ...)` re-inserts the recorded attrs into the game env so submatchers resolve (`Game.hs` ~3630).

Note `EnemyHealthActual` is *printed* health while the record holds *modified* health — they can differ on a health-modified enemy.

**Why:** `getDefeatedEnemyHealth` projected the live entity, so Bounty computed health 0, skipped its allocation prompt and granted nothing (issue #5148, fixed by falling back to the record).

**How to apply:** any card reading a defeated enemy's fields from a window must go through the recorded data, not `field`/`getEnemyField`. Beware when testing this: the test harness does not hit either `clearRemovedEntities` call site, so a defeated enemy stays queryable and a naive test passes without the fix — use `QuietlyRemoveFromGame` (a real `deleteMap`) to reproduce, and assert the projection is `Nothing` so the test cannot pass vacuously. See `tests/Arkham/Helpers/EnemySpec.hs`.
