---
title: project_enemy_attack_asset_target
description: "Enemy attacks targeting an asset \"as if an investigator\" (Key Locus) must be real EnemyAttacks; engine asset-target support"
---

Enemy attacks against an **asset** treated "as if it were an engaged investigator" (Dogs of War's Key Locus, act `RabbitsWhoRunV1`) must flow through a real `InitiateEnemyAttack`/`EnemyAttack`, not a direct `dealAssetDamage`. Before issue #4934 the locus "attack" was a `ScenarioSpecific "enemyAttacked"` → `dealAssetDamage` shortcut, so no `Window.EnemyAttacks` opened and **Dodge / other attack-cancellers couldn't fire**.

Three coupled pieces make an asset a valid attack target:
- `Arkham/Enemy/Runner.hs` `PerformEnemyAttack` only handled `SingleAttackTarget (InvestigatorTarget …)` and `MassiveAttackTargets` (else `error`). Added a `SingleAttackTarget (AssetTarget aid)` case → `DealAssetDamageWithCheck aid (EnemyAttackSource enemyId) (healthDamage+sanityDamage) 0 True` (horror folded into damage). Must NOT re-push `ScenarioSpecific "enemyAttacked"` (the act handler that initiates the attack would loop).
- `Arkham/Helpers/Window.hs` `Matcher.EnemyAttacks` / `EnemyAttacksEvenIfCancelled` only matched `InvestigatorTarget who`. Added asset case: gate on `aid <=~> AssetAt (locationWithInvestigator iid)`, then `matchWho iid iid whoMatcher` (use the reacting investigator as its own `who`) — makes "an investigator at your location" matchers (Dodge) fire, while `NotYou`-style stay false.
- The act just pushes `InitiateEnemyAttack $ enemyAttack enemy enemy locus` per Key Locus.

Note the Chapter-1 vs Chapter-2 "as if" distinction: this broad interpretation (asset is an investigator for ALL purposes of the attack) is correct for Chapter 1 content (Scarlet Keys, `09xxx`); Chapter 2's narrower "as if" would not extend Dodge here. Relates to [[project_basic_attack_enemy_source]] (attack source scoping) and [[project_simultaneous_damage_window_targets]].
