---
name: project_attackisvalid_is_the_direct_enemyattack_gate
description: "CannotBeAttackedBy is filtered on EnemyWillAttack only; retaliate/alert push EnemyAttack directly, so attackIsValid in Arkham/Attack.hs is the single shared gate for those"
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

**How to apply:** `attackIsValid` is now the one place that vets a direct `EnemyAttack`. Any
new "cannot attack" / "cannot be attacked by" style restriction belongs there, not in
`EnemyWillAttack` alone — otherwise it silently only covers the enemy phase. Note
`Enemy/Runner.hs` (`ForInvestigator/Do EnemiesAttack`) also tests the enemy-side
`CannotBeAttacked` modifier against *investigator* modifiers, which is a separate latent
mismatch. Related: [[project_basic_attack_enemy_source]], [[project_enemy_basic_abilities_load_bearing_seam]].
