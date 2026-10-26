---
name: project_attackisvalid_is_the_direct_enemyattack_gate
description: "CannotBeAttackedBy and CannotAttack are filtered on EnemyWillAttack/InitiateEnemyAttack only; retaliate/alert push EnemyAttack directly, so attackIsValid in Arkham/Attack.hs is the single shared gate for those"
metadata:
  type: project
---

Two different entry points create an enemy attack, and only one of them used to honour
"that enemy cannot attack you":

- `EnemyWillAttack` (`Arkham/Game/Runner.hs`) — the enemy-phase path. It filters the target
  against `CannotBeAttackedBy` before folding the attack into `EnemyAttacks`.
- **A direct `push $ EnemyAttack details`** — retaliate (`Failed (Action.Fight, …)`), alert
  (`Failed (Action.Evade, …)`) and `EnemyAttackIfEngaged`, all in `Arkham/Enemy/Runner.hs`.
  These skip that filter entirely; their only gate is `attackIsValid` (`Arkham/Attack.hs`),
  which checked readiness / `CanRetaliateWhileExhausted` and nothing else.

That gap is #5583: evading Elokoss, Mother of Flame applies
`roundModifier … iid (CannotBeAttackedBy (be attrs))` **and readies her**, so a failed fight
in the same round retaliated anyway. Same hole applied to On the Lam, the Atlach-Nacha legs,
The Contessa, Yig's Mercy, Another Way.

Fixed by making `attackIsValid` require `canBeAttackedBy details.enemy` for
`details.investigator` (new helper exported from `Arkham/Attack.hs`), plus gating the
retaliate/alert push sites on the same check so the `log.retaliate` line isn't printed for an
attack that won't happen.

The **enemy-side** half of the same hole is #5909: Hit Van ("cannot attack during the same
phase it moved via its hunter keyword") applies `CannotAttack` to itself, and `CannotAttack`
was checked at `InitiateEnemyAttack`, `initiateEnemyAttack` (`Message/Lifted.hs`) and
`ForInvestigator/Do EnemiesAttack` — but **not** on the direct-push path, so a failed fight
retaliated through it. Same gap for Asylum Gorger, Aquatic Abomination, Swift Byakhee, Worth
His Salt, Dagon Deep in Slumber, Cavern Moss, James "Cookie" Fredericks and the `CannotAttack`
acts. `getCannotAttackNow` (which also handles the phase-scoped
`CannotAttackDuringEnemyPhase`) moved out of `Enemy/Runner.hs` into `Arkham/Attack.hs`
alongside a new `enemyCannotAttackNow :: EnemyId -> m Bool`; `attackIsValid` now requires it,
and the retaliate/alert push sites test it too so `log.retaliate` is not printed for an attack
that will not happen. Note the retaliate/alert sites read `getCombinedModifiers [iid, enemy]`,
so they must **not** reuse that list for this check — `CannotAttack` is also an *investigator*
modifier (`Criteria.CanAttack`), and an investigator who cannot attack would otherwise
suppress the enemy's retaliate.

**How to apply:** `attackIsValid` is now the one place that vets a direct `EnemyAttack`. Any
new "cannot attack" / "cannot be attacked by" style restriction belongs there, not in
`EnemyWillAttack` alone — otherwise it silently only covers the enemy phase. Note
`Enemy/Runner.hs` (`ForInvestigator/Do EnemiesAttack`) also tests the enemy-side
`CannotBeAttacked` modifier against *investigator* modifiers, which is a separate latent
mismatch. Related: [[project_basic_attack_enemy_source]], [[project_enemy_basic_abilities_load_bearing_seam]].
