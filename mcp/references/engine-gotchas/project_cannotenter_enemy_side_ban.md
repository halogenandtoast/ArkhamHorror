---
name: project_cannotenter_enemy_side_ban
description: Location-side entry bans use CannotBeEnteredBy; enemy-side ones use CannotEnter — canEnterLocation must read both or the enemy follows an investigator anywhere
metadata:
  type: project
---

There are two unrelated modifiers for "this enemy may not be at this location":

- **`CannotBeEnteredBy EnemyMatcher`** — lives on the *location*, naming which enemies are barred.
- **`CannotEnter LocationId`** — lives on the *enemy*, naming which locations it is barred from.
  This is what enemies with a terrain restriction use: Slitherer in Darkness
  (`modifySelf a $ map CannotEnter unflooded`), Moon Lizard (non-Cave), Ravenous
  Grizzly (non-Wilderness).

`Arkham.Helpers.Enemy.canEnterLocation` gathers modifiers from *both* the location
and the enemy, but until 2026-07 it only matched `CannotBeEnteredBy` and
`CannotMove`. Enemy-side `CannotEnter` was silently ignored, so a restricted enemy
engaged with an investigator followed them into a forbidden location instead of
disengaging (issue: Slitherer following onto the unflooded Moving Platform).

The *following* logic itself was already correct: `handleDoResolveMovement`
(`Investigator/Runner/Movement.hs`) partitions engaged enemies into followers and
disengagers on `canEnterLocation`, and pushes `DisengageEnemy` for the latter
**before** the investigator leaves — `DisengageEnemy` re-homes a threat-area enemy
to the engaged investigator's *current* location, so ordering matters. Anything
that wants "stays behind when you move" only has to make `canEnterLocation` return
False.

`Arkham.Location.Runner` used to define a second, weaker `canEnterLocation` that
read only the location's modifiers. It was dead (not in
`Location.Import.Lifted`'s explicit re-export list, no direct callers) and was
removed so the two cannot diverge again.

Related: [[project_deferred_enemymove_superseded]], [[project_after_enter_engagement_timing]]
