---
name: project_enemyspawns_window_carries_placement
description: Window.EnemySpawns carries a Placement (not a LocationId) and the matcher takes a PlacementMatcher, so a Concealed enemy spawning into the shadows still opens one spawn window (#5649)
metadata:
  type: project
---

`Window.EnemySpawns EnemyId Placement` and `Matcher.EnemySpawns Timing PlacementMatcher
EnemyMatcher` (both carried a `LocationId`/`Where` before #5649). `PlacementMatcher` lives in
`Arkham.Matcher.Placement`: `AnyPlacement`, `PlacementAt LocationMatcher` (resolves the
placement to a location, so a threat area and an attachment resolve to their host's),
`PlacementIs Placement`, `PlacementOneOf`, `PlacementMatchAll`, `NotPlacement`. Both types
have a FromJSON fallback that reads the old shape as `AtLocation lid` / `PlacementAt lm`.

**Why:** `Concealed X` enemies take `SpawnPlaced InTheShadows`
(`Arkham.Enemy.Types`), and `placementLocation InTheShadows = Nothing`. The old
`Do (EnemySpawn)` `SpawnPlaced` branch fell through to a bare `PlaceEnemy` on `Nothing` — no
spawn window and no `EnemySpawned` (so `details.after` was dropped too). Only
`Window.EnterPlay` fired, from `PlaceEnemy`, so Darrell's Kodak (keyed on `EnemySpawns`)
could never photograph an enemy entering the shadows. The rules say the investigator
*spawns* the enemy into the shadows, "in play but not at any location".

**How to apply:**
- Translate a card by its printed text: "after an enemy **enters play** / spawns" is
  `AnyPlacement`; "spawns **at** X" / "enters a location" is `PlacementAt X`.
- `PlaceEnemy`'s `handlePlacement` now skips the `EnterPlay` windows when
  `enemySpawnDetails` is set, because `Matcher.EnemyEntersPlay` unions `EnterPlay` and
  `EnemySpawns` — emitting both made every "after this enemy enters play" forced ability
  fire twice on the `SpawnPlaced`-with-a-location path (Rise of the Elder Things, swarm
  creation). Don't reintroduce a second window for one spawn.
- The `#when` half comes from the `EnemySpawn` handler, the `#after` half from
  `Do (EnemySpawn)`; a locationless in-play spawn needs both added by hand.
- An out-of-play `SpawnPlaced` (the pursuit zone) still gets no spawn window — it is not
  entering play.

See [[project_global_placement_is_everywhere]] (`Global` is another locationless in-play
placement) and [[project_arkham_replay_tool]] (`stack exec` can hand you the previous
round's binary while the watcher is still rebuilding — check the install-dir mtime).
