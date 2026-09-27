---
name: project_location_abilities_field_is_empty_for_enemy_locations
description: "field LocationAbilities returns [] for an enemy-location — read a location's basic actions from the game ability list (select), never from the entity (#5603)"
metadata:
  type: project
---

`field LocationAbilities lid` goes through `maybeLocation` (`Arkham/Game/Utils.hs`), which for an
enemy-location hands back a `toEnemyLocationProxy` wrapper whose instance is literally
`getAbilities _ = []` (`Arkham/EnemyLocation/Proxy.hs`). The proxy is deliberately inert: the real
abilities come from `HasAbilities EnemyLocationAttrs` and are folded in separately as
`enemyLocationAbilities` in `getGameAbilities` (`Arkham/Game.hs`). So the entity-level field and
the game-level ability list disagree for exactly the enemy-location case.

Issue #5603: Beguile gated its "basic investigation" option on
`selectAny $ BasicAbility <> #investigate <> AbilityOnLocation (locationWithEnemy eid)` — the game
ability list, which *does* include enemy-locations — but then executed via
`field LocationAbilities lid`, got `[]`, and hit
`error "expected exactly 1 investigate action on location"`. Moving a Grappling Spawn onto Hemlock
House's Living Foyer and taking the basic investigate crashed the game every time. Fixed by
running the same `select` in the handler as in the gate.

**How to apply:** any card that reaches for another entity's *basic* action (investigate, fight,
evade, move) must query it with `select` over `AbilityMatcher` (`BasicAbility <> #investigate <>
AbilityOnLocation ...`), not with `field LocationAbilities`. More generally: a gate and its handler
must query the same source of truth, or a matcher that quietly covers more entity kinds than the
field does turns a legal option into a crash. Related:
[[project_enemy_location_runner_must_mirror_enemy_windows]], [[project_enemylocation_discovery_gap]],
[[project_enemy_basic_abilities_load_bearing_seam]].
