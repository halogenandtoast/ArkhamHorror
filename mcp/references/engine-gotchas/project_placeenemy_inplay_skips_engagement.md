---
name: project_placeenemy_inplay_skips_engagement
description: "PlaceEnemy (AtLocation) on an already-in-play enemy checked no engagement, so card effects that *place* an enemy left it ready and unengaged in the investigator's location"
metadata:
  node_type: memory
  type: project
---

`PlaceEnemy eid (AtLocation lid)` in `Arkham/Enemy/Runner.hs` has two branches. If the enemy is
**out of play** it runs the full `EnemySpawn` flow, which engages (`Will (EnemyEngageInvestigator …)`
/ `EnemyCheckEngagement`). If the enemy is **already in play** it went straight to `handlePlacement`,
which pushes `checkEntersThreatArea` (a no-op for `AtLocation`) and the after-`EnemyPlaced` window —
and checked engagement nowhere. Net effect: a ready enemy *placed* at an investigator's location by
a card effect sat there engaged with nobody, and nothing re-checked.

Real case (issue #5332): The Miskatonic Museum act 2b **Breaking and Entering** does `place eid
InTheShadows` → `ready eid` → `place eid (AtLocation restrictedHall)`. `InTheShadows` **is** an
in-play placement (`Arkham/Placement.hs`), so the last `PlaceEnemy` took the in-play branch and the
Hunting Horror never engaged. The user-visible symptom was two steps removed from the cause: **Eon
Chart (4)**'s second basic action silently offered nothing, because with the Horror unengaged there
was no performable basic *evade*.

**Why it mattered here and not before:** spawns engage through the `EnemySpawn` flow and moves engage
through `After (EnemyEntered)` (#5292). A bare `PlaceEnemy` is the third way an enemy arrives at a
location and it was the only one with no engagement hook.

**How to apply:** the in-play branch now pushes `EnemyCheckEngagement enemyId` *before*
`handlePlacement`, so engagement resolves ahead of the after-`EnemyPlaced` reaction window — same
ordering convention as movement. This is safe because `EnemyCheckEngagement` is itself fully guarded
(swarm cards, `delayEngagement`, attached, aloof/massive, **exhausted**, already-engaged,
`CannotEngage` / `CannotBeEngaged{,By}`), so deliberately-unengaged placements stay unengaged. The
intentional disengage-by-placement callers in `Game.hs handleAsIfChanges` place the enemy at a
location the investigator has *left*, so the new check finds nobody; `handleAloofChanges` already
ran `EnemyCheckEngagement` from that same preload path.

Same-shaped sites that were also silently affected: `Act/Cards/WheresBertie.hs`,
`SearchingTheUnnamable.hs`, `WalkingThroughTime.hs`, `ThePetGroupC.hs`, `SearchingForDrArmitage.hs`,
`Agenda/Cards/LostMemories.hs`, `CityOfTheGreatRace.hs`, `RestlessDead.hs`, `TheFamiliar.hs`.

Debugging lesson: the reported symptom was "Eon Chart's resolution ended early", but `--trace` showed
`DoStep 2 (ForAction Move …)` *was* processed — the continuation was intact and the missing option
was a stale game-state fact upstream. When a card "does nothing", check whether its option list is
empty for a legitimate reason before suspecting the queue.

Regression tests: `tests/Arkham/Enemy/EngagementSpec.hs` (a positive case and an exhausted-enemy
negative case).

Related: [[project_enemyentered_threat_area_placement]], [[project_after_enter_engagement_timing]],
[[project_aoo_gated_at_callsite]].
