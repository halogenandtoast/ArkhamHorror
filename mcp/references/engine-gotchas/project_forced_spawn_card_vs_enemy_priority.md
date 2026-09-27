---
title: project_forced_spawn_card_vs_enemy_priority
description: "Spawn-override modifier priority — ForceSpawn (On the Hunt etc.) beats OverwrittenSpawn (scenario rules like Dead Heat's random-location spawn)"
---

There are two spawn-override modifiers (`Arkham.Modifier`), resolved in `Arkham.Enemy.Runner` `InvestigatorDrawEnemy`:

- **`ForceSpawn SpawnAt` / `ForceSpawnLocation`** — a deliberate effect placing the enemy (On the Hunt, On the Hunt (3), Kicking the Hornet's Nest apply `ForceSpawn (SpawnEngagedWith you)` to the **card** via `searchModifiers`). Also counts as `isForcedEngagement` (forces engagement past aloof/exhausted).
- **`OverwrittenSpawn SpawnAt`** — a scenario rule that merely *replaces* an enemy's normal spawn location. Used by Dead Heat (Gnashing Teeth → `OverwrittenSpawn SpawnAtRandomLocation` for Ghoul/Risen, Empty Streets for AnyEnemy) and the core "spawns here" rules (Your House → `OverwrittenSpawn (SpawnAt (be attrs))` for Ghoul Priest, Burial Ground for Ghouls). Does **not** force engagement. No separate location variant: `SpawnAt LocationMatcher` is itself a `SpawnAt` constructor, so a location is `OverwrittenSpawn (SpawnAt <matcher>)`.

Priority: `getForcedSpawnAt mods <|> getOverwrittenSpawnAt mods` — ForceSpawn always wins. FAQ (v2.5 Q137): On the Hunt / Kicking the Hornet's Nest "spawn the searched enemy engaged with you instead of its normal spawn location." This design (issue #4918) replaced an earlier bug where the agenda used `ForceSpawn` and, since `mods = getModifiers enemyId <> getModifiers cardId` is enemy-first and `getForcedSpawnAt` took the first match, the scenario's random spawn wrongly beat On the Hunt.

Notes: the frontend decodes unknown modifier tags via a catch-all `OtherModifier`, so adding backend modifier constructors is safe there. `getModifiedSpawnAt` (the concealed-Nothing fallback branch) is only reached when neither ForceSpawn nor OverwrittenSpawn is present. Your House / Burial Ground were converted from `ForceSpawnLocation` to `OverwrittenSpawn` too, so On the Hunt overrides them as well; `ForceSpawnLocation` is now unused by cards but kept in the ForceSpawn family. Test trick (OnTheHuntSpec): add `AddKeyword Aloof` to the drawn enemy so a plain location-spawn won't auto-engage, making the `InThreatArea` assertion deterministic regardless of the random location. Related: [[project_basic_attack_enemy_source]].
