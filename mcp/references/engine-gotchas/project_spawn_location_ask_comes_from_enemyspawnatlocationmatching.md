---
name: project_spawn_location_ask_comes_from_enemyspawnatlocationmatching
description: "A card's own setSpawnAt matcher never reaches `Do (EnemySpawn)` as a matcher — `spawnAt` routes `SpawnAt` through `EnemySpawnAtLocationMatching`, and that is where the \"choose a spawn point\" ask is built"
metadata:
  node_type: memory
  type: project
---

`Helpers/Enemy.hs:75` `spawnAt eid miid (SpawnAt matcher)` pushes
`EnemySpawnAtLocationMatching miid matcher eid`, whose handler
(`Enemy/Runner.hs:2364`) selects `replaceYouMatcher activeInvestigatorId matcher`
and hands >1 hit to `spawnAtOneOf` (`Helpers/Enemy.hs:407`) — the `chooseOne`
whose labels already carry **`SpawnAtLocation lid`**. So by the time a spawn
reaches `Do (EnemySpawn …)`, `spawnAt` is `SpawnAtLocation`, never a matcher.

`Do (EnemySpawn …)`'s own `SpawnAt matcher -> chooseOrRunOne`
(`Enemy/Runner.hs:511`) is **not** dead, but it only serves a spawn a modifier
redirected (`ChangeSpawnWith` / `ChangeSpawnLocation`, pushed at
`Enemy/Runner.hs:479`). Two asks, neither covering the other.

**Why:** narrating the spawn-point prompt on `Do (EnemySpawn …)` alone compiles
clean, matches the branch that reads like the obvious one, and never fires for
any card that declares `setSpawnAt`. Caught only by reading the live game's
pending question, which carried `SpawnAtLocation` choices.

**How to apply:** for anything keyed on "where will this enemy spawn", match
`SpawnMessage (EnemySpawnAtLocationMatching_ miid matcher eid)` and resolve
`You` the way the handler does — via `select ActiveInvestigator`, not
`getActiveInvestigatorId`, whose `selectJust` throws. Headless check: the ask
is already built by the time an export parks its queue (the trace resumes at
`After (SpawnMessage (EnemySpawnAtLocationMatching_ …))`), so re-create it with
an `arkham-replay --answers` `Raw` injection of that message rather than
`--undo`. See [[project_gold_glowing_enemy_is_an_unplaced_spawn_ghost]] and
[[project_spawnplaced_never_raised_the_enters_windows]].
