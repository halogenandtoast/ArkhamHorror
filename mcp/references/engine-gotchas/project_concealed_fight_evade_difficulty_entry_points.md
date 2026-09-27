---
name: project_concealed_fight_evade_difficulty_entry_points
description: "Concealed mini-cards compute fight/evade difficulty at four separate sites in Concealed/Runner.hs; only the two UseThisAbility ones folded in EnemyFight/EnemyEvade modifiers, so card-effect fights (AttackEnemy) and evades (TryEvadeEnemy) silently dropped them"
metadata:
  node_type: memory
  type: project
  modified: 2026-08-01T21:05:00.000Z
---

A concealed mini-card (`Arkham.Campaigns.TheScarletKeys.Concealed.Runner`) has **four** entry
points that build a skill-test difficulty, and they are not shared:

| handler | reached by |
|---|---|
| `UseThisAbility … AbilityAttack` | the basic fight action on the mini-card |
| `UseThisAbility … AbilityEvade` | the basic evade action |
| `AttackEnemy` (`DefaultChooseFightDifficulty`) | any card effect that fights — `chooseFightEnemy` / `chooseFightEnemyEdit` |
| `TryEvadeEnemy` | any card effect that evades — `chooseEvadeEnemy` |

The base difficulty is the **location's shroud**, not an enemy's fight/evade value — concealed cards
have no printed stats. Cards that raise the difficulty (Rambling Route 09747a/b/c, Cliffs of
Insanity) express it as `EnemyFight n` / `EnemyEvade n` modifiers on a `ConcealedCardTarget`, which
only matter if the handler explicitly folds them into the calculation. The two `AttackEnemy` /
`TryEvadeEnemy` handlers didn't, so Rambling Route's +2 vanished for anyone fighting with a weapon
asset instead of the bare fight action (#5329, reported against Isabelle's Twin .45s ability 2).

**Why:** the difficulty is a `GameCalculation` baked at test-creation time, not a live modifier
lookup — nothing downstream re-consults the concealed card's modifiers, so a handler that omits the
fold loses the bonus permanently.

**How to apply:** all four now go through `concealedLocationFor` + `concealedTestDifficulty`
(`concealedFightBonus` / `concealedEvadeBonus`) in the same module. Any new concealed fight/evade
path must use them too. `CalculatedChooseFightDifficulty` is deliberately left alone: an explicitly
supplied difficulty replaces the enemy's value entirely, matching normal-enemy behaviour.

Note also that "at" a location, for a concealed card in the shadows, means **in a grid position
adjacent to it** — see `LocationConcealedCards` in `Game.hs`, which unions
`locationConcealedCards` with everything `InPosition` in `adjacentPositions`. So Rambling Route's
`modifySelect` over `adjacentPositions` is correct scope, not a bug.

Repro/verification: `arkham-replay <export> --undo 3 --answers …` rewinds to the reaction-ability
window, re-takes the two choices, and `.gameSkillTest.difficulty` goes from a bare
`LocationMaybeFieldCalculation` to `SumCalculation [Fixed 2, …]`. Related:
[[project_skilltestresultvaluemodifier_additive_delta]], [[project_omnipotent_matcher_exclusion]].
