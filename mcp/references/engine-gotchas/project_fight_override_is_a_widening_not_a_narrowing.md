---
name: project_fight_override_is_a_widening_not_a_narrowing
description: "A CanFightEnemyWithOverride matcher replaces the standard fight criteria, it does not narrow the enemy set — the as-if-enemy gate must look through it or Mist-Pylons vanish from ignore-Aloof attacks (#5676)"
metadata:
  type: project
---

`ChooseFightEnemy` (`Arkham/Investigator/Runner.hs`) only offers targets that are attackable
*as if* an enemy (Mist-Pylons, Key Loci, concealed cards) when the fight is unrestricted. The
gate used to be a bare `coveredByAnyInPlayEnemy choose.matcher`, which falls through to
`_ -> False` for **every** `CanFightEnemyWithOverride`.

But an override *replaces the standard fight criteria*; it does not narrow the enemy set. That
is exactly how a card spells "ignore Aloof":

- Longbow (3) — `ignoreAloofFightOverride AnyEnemy`
  → `CanFightEnemyWithOverride (CriteriaOverride (EnemyCriteria (ThisEnemy (EnemyAt YourLocation <> CanBeAttackedBy You))))`
- British Bull Dog (2) — `CriteriaOverride canFightIgnoreAloof`
  → `Criteria [CanAttack, OnSameLocation, EnemyCriteria (ThisEnemy (CanBeAttackedBy You))]`

Note the two shapes differ: `fightOverride`/`ignoreAloofFightOverride` wrap an **EnemyMatcher**,
while `canFightIgnoreAloof`/`canFightCriteria` are **Criterion**s. Anything reasoning about
overrides has to handle both.

Issue #5676: standing on a revealed Mist-Pylon, Longbow (3)'s attack offered only the real
enemy — the pylon silently disappeared, while the plain fight action still offered it. Runic
Axe had the same bug and had been patched by hand with the `canMoveToConnected` escape hatch
(#5657); Longbow (3) and British Bull Dog (2) were never covered. Fixed with
`fightOffersAsIfEnemyTargets` (`Arkham/Criteria.hs`), which unwraps the override and treats a
criterion built only from the standard restrictions (`OnSameLocation`, `CanAttack`,
`EnemyAt YourLocation`, `CanBeAttackedBy You`) as unrestricted. `canMoveToConnected` stays —
it also widens *where* the as-if-enemy selects look, which is a separate job.

**Why it can't live next to `coveredByAnyInPlayEnemy`:** `Arkham/Matcher/Enemy.hs` imports
`Matcher.Location` and `Matcher.Investigator` `{-# SOURCE #-}`, so `LocationMatcher` and
`InvestigatorMatcher` are abstract there — it cannot see inside `EnemyAt` or `CanBeAttackedBy`,
let alone `Criterion`. `Arkham/Criteria.hs` sees all of them.

**How to apply:** when a fight target "isn't offered", check whether the card sets an override
matcher before suspecting the entity. Narrowing overrides (Service Revolver's `EnemyWithId`,
Summoned Servitor's `EnemyAt (locationWithAsset …)`, Enchanted Bow (2), Guerrilla Tactics) must
stay excluded; widening ones must not. Pure unit cases live in
`tests/Arkham/Matcher/CoveredByAnyInPlayEnemySpec.hs`. The evade side
(`ChooseEvadeEnemy`, concealed mini-cards) still uses the bare `coveredByAnyInPlayEnemy` and
has the same latent gap. Related:
[[project_as_if_enemy_targets_have_three_flavours]],
[[project_concealed_target_breaks_enemy_scoped_matchers]].
