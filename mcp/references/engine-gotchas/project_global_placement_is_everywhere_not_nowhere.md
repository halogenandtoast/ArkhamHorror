---
name: project_global_placement_is_everywhere_not_nowhere
description: Global placement means "co-located with everyone", so a Global enemy passes OnSameLocation and is fightable from anywhere; use InPosition for an enemy that is at no location
metadata:
  type: project
---

`Placement.Global` reads as *everywhere*, not *nowhere*. The only place either
placement is actually distinguished is `Arkham.Helpers.Placement.onSameLocation`:

```haskell
Global      -> pure True
InPosition _ -> pure False
```

Everything else treats the two identically — `placementLocation`/`EnemyLocation`
give `Nothing`, `isInPlayPlacement` is `True`, `placedInThreatArea` and
`placementToTarget` are `Nothing`.

That one line is load-bearing. `canFightCriteria` (`Arkham.Criteria`) is
`OnSameLocation <> ThisEnemy (CanBeAttackedBy You) <> CanAttack`, and the basic
fight ability in `Enemy/Types.hs` is restricted on it, so **every investigator
passes the location gate for a Global enemy from anywhere on the map**. That is
correct for Azathoth (Before the Black Throne) and Subject 8L-08 (The Blob),
which are meant to be reachable; it is wrong for an enemy that is simply at no
location.

Note the asymmetry that makes this easy to miss: the basic **engage** ability
explicitly negates it (`Negate (EnemyCriteria $ ThisEnemy $ EnemyWithPlacement
Global)`) and **evade** requires `EnemyIsEngagedWith You`, so those two are
already off. Fight is the only basic action with no Global guard, which is why
the bug presents as "the enemy is fightable but not engageable".

Circus Ex Mortis' Harm's Way hit this: the four Towering Dark Young are at no
location (their cards have no printed health at all, and the one attack against
them reads "as if it were at your location"), but were placed `Global` for
rendering, making all four fightable from anywhere. Fixed by placing them
`InPosition (Pos …)` on the empty diagonal corners around Ringmaster's Trailer at
`Pos 0 0`, with `Arkham.Location.Grid.gridLabel` deriving the `EnemyAsSelfLocation`
grid-area string from the same `Pos` (2026-09-04).

`InPosition` is the right placement for "in play, on the map, at no location".
The frontend buckets an enemy by `asSelfLocation` and only excludes `OutOfPlay`
(`Scenario.vue`), so switching from `Global` costs no rendering.

Related: [[project_enemy_location_runner_must_mirror_enemy_windows]],
[[project_attackisvalid_is_the_direct_enemyattack_gate]]
