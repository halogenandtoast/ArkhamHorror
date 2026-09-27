---
title: Stalking Hybrid only engages, moves toward, and spawns toward its Vale Lantern prey
date_added: 2026-06-17
source: Designer ruling (Feast of Hemlock Vale)
affects:
  - Stalking Hybrid
  - Vale Lantern
  - prey
  - OnlyPrey
---

# Stalking Hybrid only engages, moves toward, and spawns toward its Vale Lantern prey

> Because the Stalking Hybrid has "Prey – investigator with the Vale Lantern only," it only
> engages and moves toward the investigator that meets that instruction. If drawn by an
> investigator that does not match this instruction, it spawns unengaged at that investigator's
> location, and will only automatically engage its prey.

The "only" in the Prey instruction makes it an *exclusive* prey restriction: the Stalking Hybrid
never automatically engages a non-prey investigator. When a non-prey investigator draws it, it
still spawns at that investigator's location (default spawn), but **unengaged**. As a Hunter it
then only moves toward — and engages — the investigator that controls the Vale Lantern.

## Affected cards / systems

- Stalking Hybrid (10625) — `backend/arkham-api/library/Arkham/Enemy/Cards/StalkingHybrid.hs`
- Vale Lantern (10610/10611) — the prey-defining asset
- `OnlyPrey` prey matcher — `backend/arkham-api/library/Arkham/Enemy/Runner.hs`

## Implementation status

- **Stalking Hybrid (10625)**: ✅ no card change needed. It declares its prey with
  `setOnlyPrey (ControlsAsset $ AssetWithTitle "Vale Lantern")`, i.e. `enemyPrey = OnlyPrey (...)`.
- **Hunter movement / engagement-at-location**: ✅ already matched. The `OnlyPrey` branch of
  `HunterMove` targets `NearestLocationToLocation` of the prey, `wantsToHunt` uses
  `getActualAvailablePrey`, and the `SpawnAtLocation` engagement gate only pushes
  `EnemyEngageInvestigator` for `iid \`elem\` preyIds`. So once spawned it only moves toward and
  engages its prey.
- **Spawn-on-draw**: ✏️ **fixed — this was a bug.** The default draw path
  (`InvestigatorDrawEnemy` in `backend/arkham-api/library/Arkham/Enemy/Runner.hs`) chose
  `SpawnEngagedWith (InvestigatorWithId drawer)` based only on `InvestigatorCanBeEngagedBy` and
  `AloofEnemy` — it ignored the prey restriction entirely, so a non-prey drawer was engaged on
  spawn. Added an `OnlyPrey` check: if the enemy has an exclusive prey and the drawer is not prey,
  it now spawns via `SpawnAtLocation` (the prey-aware path), spawning unengaged unless its prey is
  present at that location. This also corrects the other `setOnlyPrey` enemies (Preying Byakhee,
  Feline Hybrid, Namer of the Dead, etc.), whose cards likewise read "Prey – X only."
- **Test**: added `StalkingHybridSpec` driving the actual `InvestigatorDrawEnemy` path and asserting
  the Hybrid spawns unengaged at a non-prey drawer's location.
  (`backend/arkham-api/tests/Arkham/Enemy/Cards/StalkingHybridSpec.hs`)
  Note: an earlier version of this test used the `spawnAt` helper, which routes through
  `SpawnAtLocation` and so passed even with the bug present — it never exercised the draw path.
