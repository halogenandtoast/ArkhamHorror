---
name: project_moveuntil_stops_short_engage_drags
description: MoveUntil silently stops short when the investigator can't move; a same-batch engageEnemy then drags the enemy across the board to them
metadata:
  type: project
---

`MoveUntil lid (InvestigatorTarget iid)` walks the investigator **one hop per
re-push** (`handleMoveUntil`, `Arkham/Investigator/Runner/Movement.hs`). Every hop
re-checks `CannotMove`, and when it fails the handler **no-ops silently** — no
message, no window, nothing in `--trace`. So a movement that started legally can
end anywhere along the path.

Anything queued alongside `moveUntil` therefore runs whether or not the
investigator arrived. That matters most for `engageEnemy`, because
`Do (EngageEnemy …)` (`Arkham/Enemy/Runner.hs`) is
`PlaceEnemy eid (InThreatArea iid)` + `EnemyEntered eid <investigator's location>`
— engagement **pulls the enemy to you** from wherever it was. The only guard is
`canEnterLocation`; there is no "already at your location" check, by design
(spawn-engaged-with-prey depends on the pull).

**#5353**: The Grapevine (60359) pushed `moveUntil` and `engageEnemy` in one
batch. Whispers in Your Head (Dread) (`03084b`, *"you cannot move more than once
each turn"*) blocked hop 2, and the unconditional engage teleported The Man in
the Pallid Mask out of the Green Room into the Lobby.

**How to apply:** never queue a follow-up alongside `moveUntil` if it assumes
arrival. Defer it behind the movement — `forTarget_ eid msg` plus a
`ForTarget (EnemyTarget eid) …` case — and re-check position at resolution time
with `enemyAtLocationWith iid`. Same trap applies to any post-move rider (damage,
clue discovery, "at that location" effects), not just engagement.

Related: [[project_after_enter_engagement_timing]],
[[project_placeenemy_inplay_skips_engagement]],
[[project_enemyentered_threat_area_placement]].
