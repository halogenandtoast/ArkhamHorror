---
title: project_hunter_group_stale_targets
description: "HandleGroupTargets/HunterGroup re-projects every remaining target's fields on each re-entry, so a hunter that leaves play while an earlier hunter's move resolves crashes getEnemy with 'Unknown enemy' (#5421)"
---

Hunter/patrol movement is batched. `HuntersMove` (`Enemy/Runner.hs` ~922-960) pushes one
`HandleGroupTarget HunterGroup (EnemyTarget eid) [...]` per hunting enemy; consecutive ones merge
into a single `HandleGroupTargets` whose `targetMap` holds **every** hunter for the phase
(`Game/Runner.hs` ~2190-2207).

The group then resolves **one target at a time**: each option is
`TargetLabel target (msgs <> [HandleGroupTargets Manual k (mapFromList rest)])`, so the message
re-enters the runner after each individual move — and on every re-entry it **recomputes**
`validTargetsForKey` by calling `getModifiedKeywords eid` (and `getAttrs @Enemy eid` in the
`Keyword.Hunter` branch) for each target still in the map.

Those projections are not guarded. Anything that removes a hunter from play *while an earlier
hunter's move resolves* leaves a dangling `EnemyTarget` in the map, and the next re-entry throws:

```
Safe.fromJustNote Nothing, Unknown enemy: <eid>
  getEnemy, called at Arkham/Game.hs   (field)
  field, called at Arkham/Helpers/Enemy.hs:159   (getModifiedKeywords)
  getModifiedKeywords, called at Arkham/Game/Runner.hs:2219
```

Removal mid-batch is easy to hit, not exotic:

- a location's `EnemyEnters #after` forced ability that damages/defeats the entering enemy —
  #5421 was three Hydra's Brood (`07334`) hunting into a Black-Keyed **Sunken Halls** (`07321`,
  ability 2 = "deal 2 damage to it"), each defeated on arrival;
- a `WouldMoveFromHunter` reaction (the hunter branch wraps its move in `wouldWindows`);
- any player card that defeats or removes an enemy off a movement/engagement window.

Note the defeated enemy can be gone *entirely*: it is not only `defeated`/`OutOfPlay RemovedZone`
in the entity map, it is eventually GC'd out of `entitiesL . enemiesL`, so `getEnemy`'s
removed-entity fallback (`Game/Utils.hs` `maybeEnemy`) misses too — see
[[project_removed_entities_cleared]].

**The fix (#5421)** guards the filter before any field projection:

```haskell
eid <- hoistMaybe target.enemy
enemyAttrs <- toAttrs <$> MaybeT (project @Enemy eid)
guard $ isInPlayPlacement enemyAttrs.placement && not enemyAttrs.defeated
kws <- lift $ toList <$> getModifiedKeywords eid
```

`project @Enemy` is the non-throwing counterpart of `getEnemy`, but it *can* still surface a
just-removed entity from the removed-entity cache, so the placement/defeated checks are load
bearing — don't drop them. All three consumers (`NoAutoStatus`, `Manual`, `Auto`) build their
`rest` maps from the filtered map, so a departed enemy is dropped from the batch permanently.

Note `selectAny (EnemyWithId eid)` is **not** the right guard here: the plain enemy matcher appends
`EnemyWithoutModifier Omnipotent` (see [[project_omnipotent_matcher_exclusion]]), and the
`IncludeOmnipotent` escape hatch skips `restrictToInPlayZones` entirely — so it filters on the wrong
axis in both directions. Enemy-locations are safe with the placement check: their proxy
(`EnemyLocation/EnemyProxy.hs`) reports `AtLocation ela.id`.

**How to apply:** any queue construct that stores entity ids and re-reads their fields across
several re-entries (group handlers, `MoveUntil`, batched attacks) must re-verify the entity is still
in play on each pass — the id was valid when the batch was built, not when it is consumed.
Related: [[project_enemy_movement_placement]], [[project_removed_entities_cleared]],
[[project_omnipotent_matcher_exclusion]].
