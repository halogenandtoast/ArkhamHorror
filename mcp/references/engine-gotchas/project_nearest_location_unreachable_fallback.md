---
name: project_nearest_location_unreachable_fallback
description: NearestLocationToYou/NearestLocationTo fall back to all matching locations when none is reachable (FAQ); NearestLocationToLocation deliberately does not, because it backs hunter moveTowards
metadata:
  type: project
---

FAQ v2.5: a location with no valid path **is** the "nearest" — but only when no eligible
location has a valid path. Before #5638, `NearestLocationToYou` / `NearestLocationTo`
returned only what `getShortestPath` found, so an unreachable-but-eligible location could
never qualify. `Arkham.Game.getNearestLocations` (next to `getShortestPath`) now returns
the candidate list when the BFS comes up empty, and both matchers route through it.

**Why:** Slitherer in Darkness (11605, *Spawn — nearest flooded location*) was discarded on
draw in The Grand Vault. That scenario draws no location connections at all — the only
connectivity is the `ConnectedToWhen` modifier the Moving Platform grants its four grid
neighbours — so with the Platform swapped away from the flooded row, every flooded location
sat in a separate component and the matcher returned `[]` → `noSpawn` → discard.

**How to apply:** `NearestLocationToLocation` is deliberately NOT given the fallback — it
backs hunter `moveTowards` in `Enemy/Runner.hs`, and the sibling FAQ entry says an effect
that tracks a path does not move when there is no valid path. The enemy analogue is opt-in
instead (`NearestEnemyToFallback`, `NearestEnemyToLocationFallback`). Card code that rolls
its own "nearest" via `getDistance` has the same hole — Slitherer's ability 1 did — so add
the same `Nothing -> allCandidates` branch there. See
[[project_global_placement_is_everywhere]] and [[project_flood_level_is_per_investigator_not_per_location]].
