---
name: project_enemy_location_disposal_must_precede_after_windows
description: "An enemy-location's removal + relocation is its DISPOSAL, so it belongs in closeDefeat's disposal slot, not After (Defeated); EnemyWasAt YourLocation then needs displacedYourLocation to survive the relocation (#5980)"
metadata:
  node_type: memory
  type: project
  originSessionId: b10694b4-81b7-4c0f-b80e-4156d6a24ed2
  modified: 2026-11-09T23:45:40.226Z
---

`Arkham/EnemyLocation/Runner.hs` used to hand the scenario its
`EnemyLocationDefeated` (victory display, grid collapse, relocate everything that
stood on the room) on `After (Msg.Defeated ...)` — i.e. **after**
`Defeat.closeDefeat` had already fired both `#after EnemyDefeated` and
`#after IfEnemyDefeated`. The old comment defended this: otherwise
`EnemyWasAt YourLocation` could never match a room you were standing on.

The cost was worse. Every `#after` defeat reaction resolved with the investigator
still standing on the room they had just brought down. In #5980 Grete Wagner (3)
("discover 1 clue at your location or a connecting location") was never offered,
because her `canDiscoverCluesAt (orConnected NotForMovement YourLocation)` saw
the defeated Living Washroom (0 clues) as "your location", while every connecting
location with clues held no investigator — which act "Strange Infestation"
(10529) forbids discovering from. Once the relocation resolves first she lands on
a Living Bedroom holding 1 clue and the reaction is offered.

**Why:** the scenario hand-off IS this entity's disposal, the same way discard or
victory is an enemy's, and the rules put removal-from-play consequences
*simultaneously with* the card leaving play
(`mcp/references/rules/glossary/leaves_play.md`) — inside the defeat, not after
it. So it goes in `closeDefeat`'s `disposal` argument, which already sits between
`#after EnemyDefeated` and `#after IfEnemyDefeated`.

Moving it there does relocate you before every `#after` reaction, and the rules
folks are clear that you are *still* in the window of "after you defeat an enemy
at your location" even though the enemy and its room have both gone to the
victory display. So `Arkham/Game.hs` pairs the reorder with
`displacedYourLocation`: an `EnemyWasAt` query widens `YourLocation` by the
investigator's `previousLocation` **when that location has actually left play**.
A removed location cannot be `select`ed at all, so without it Autopsy Report (3),
Decorated Skull (+3), Dissection Tools, Microscope (+4) and Tommy Muldoon (2)
would silently stop firing on an enemy-location. The liveness guard is what keeps
an ordinary move from widening the match.

**How to apply:** when an `#after` defeat reaction misreads the board on an
enemy-location, check the *ordering* in `EnemyLocation/Runner.hs` before the
matcher — the window itself fires fine (see
[[project_enemy_location_runner_must_mirror_enemy_windows]]). And when a
reaction keys on where something *was*, remember that `select` is the liveness
test: anything the defeat removed is unreachable by the time `#after` resolves,
so the matcher needs an id it kept, not an entity it can look up. Related:
[[project_enemylocation_discovery_gap]],
[[project_objective_on_victory_enemy_defeat_loses_the_xp]].
