---
name: project_spawnplaced_never_raised_the_enters_windows
description: "A SpawnPlaced spawn skipped EnemyEnters / EnemyEntersYourLocation entirely — those windows live only in the EnemyEntered handler, and only SpawnEngagedWith / SpawnAtLocation pushed it (#5787)"
metadata:
  node_type: memory
  type: project
  originSessionId: 9ec364e0-e031-4165-ae54-3bbb4f54f826
  modified: 2026-09-29T21:57:25.329Z
---

`Window.EnemyEnters` and `Window.EnemyEntersYourLocation` are raised in exactly two
places: the `EnemyEntered` and `EnemyEnteredFollowing` handlers in
`Arkham/Enemy/Runner.hs`. `Do (EnemySpawn)` pushed `EnemyEntered` from its
`SpawnEngagedWith` and `SpawnAtLocation` branches but **not** from `SpawnPlaced`,
which only pushed `PlaceEnemy` — and `PlaceEnemy` on an enemy already in play runs
`handlePlacement`, which emits `EnemyPlaced`/`EnterPlay` and nothing else. So an
enemy that spawned via a placement silently skipped the enters windows.

Rise of the Elder Things (08697) hit this: its text says "spawn it engaged with you"
but it used `createEnemyWithPlacement_ … (InThreatArea iid)`, so Gather Intel (12036,
"Fast. Play when an enemy enters your location") was never offered to anyone at the
location — and being a `#when` fast window, the whole window was lost (#5787). Same
hole for Pursued, Cash Cart, Shard of Y'chlecht, Altered Beast, Knight of the Inner
Circle, Allosaurus, Brood of Yog-Sothoth, and the homebrew placement DSL
(`Homebrew/DarkMatter/Helpers.hs`).

Fixed two ways: the card now spawns with `createEnemy_ card iid` (`SpawnEngagedWith`
via `IsEnemyCreationMethod InvestigatorId`), and `Do (EnemySpawn)`'s `SpawnPlaced`
branch pushes `[PlaceEnemy, EnemyEntered eid lid, EnemySpawned]` for an
`InThreatArea` placement.

**Why:** the enters windows are invisible from the spawn branch you are reading —
nothing in `SpawnPlaced` or `PlaceEnemy` mentions them, so the gap looks like the
card's fault or the trigger's.

**How to apply:**
- Pushing `EnemyEntered` in a spawn branch means **dropping** the hand-rolled
  `#after EnemySpawns` window: `After (EnemyEntered)` emits
  `mkAfter (Window.EnemySpawns eid (AtLocation lid))` itself while
  `enemySpawnDetails` is set. Keeping both double-fires every "after this enemy
  spawns / enters play" ability — the trap the `handlePlacement` `EnterPlay` guard
  already documents.
- The window then reports `AtLocation lid` rather than the placement, which matches
  the `#when` half the non-`Do` `EnemySpawn` handler already pushes. Safe because no
  card matches on `PlacementIs` and `PlacementAt` resolves a threat area to its
  host's location.
- Scope it to `InThreatArea`. Every other placement that `placementLocation`
  resolves holds the enemy somewhere other than on the location: `AsSwarm` makes
  `EnemyEntered` re-enter the **host** (the handler forwards it), and an attachment
  or a vehicle only borrows its host's location.
- A card whose text says "spawn it engaged with you" should use the
  `SpawnEngagedWith` creation method, not a threat-area placement — the placement
  path also skips `canSpawnInLocation`, the `Will (EnemyEngageInvestigator)` window,
  forced-engagement modifiers and swarm placement.

Related: [[project_forced_window_who_must_be_you]],
[[project_yourlocation_window_handler_fanout]],
[[project_after_enter_engagement_timing]], [[project_arkham_replay_tool]].
