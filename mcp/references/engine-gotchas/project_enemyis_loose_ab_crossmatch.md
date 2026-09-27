---
title: project_enemyis_loose_ab_crossmatch
description: enemyIs/EnemyIs uses loose CardCode Eq that treats a/b sides as equal (88035a==88035b); same-name patrol pairs cross-match — use enemyIsExact
---

`enemyIs = EnemyIs . toCardCode`, and `EnemyIs` matching uses the loose `Eq CardCode`
(`Card/CardCode.hs:25-42`) that treats complementary sides equal: `88035a == 88035b`
(a↔b, c↔d). So `enemyIs casinoGuardA` also matches Casino Guard B.

Bit Fortune and Folly patrol (issue #5118): `HasModifiersFor` stamped per-enemy
`ScenarioModifier "<enemy>Next"` destination on the next-step location via
`selectEach (LocationWithEnemy $ enemyIs enemyCode)`. The a/b cross-match let Casino
Guard A's position stamp `casinoGuardBNext` onto B's own location → B's patrol
destination == B's location → `not_ (EnemyAt dest)` false → B never patrols.

**Why:** same-name A/B enemies (Casino Guard, Security Patrol, House Dealer, Fortune's
Shield/Dagger) share base code with a/b suffix; loose Eq collapses them.

**How to apply:** when a modifier/select must target ONE specific enemy card variant
(not its complementary sibling), use `enemyIsExact` (EnemyIsExact, exact string eq via
`cardCodeExactEq`) not `enemyIs`.
