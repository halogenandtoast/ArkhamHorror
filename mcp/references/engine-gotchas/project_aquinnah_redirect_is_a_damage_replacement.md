---
name: project_aquinnah_redirect_is_a_damage_replacement
description: "Aquinnah's \"deal that enemy's damage to any enemy, instead\" is a replacement for the attack's damage step, not an immediate effect in the #when window — it is registered on the attack (attackDamageReplacement) and resolved inside PerformEnemyAttack, so a later Dodge cancels the redirect too"
metadata:
  node_type: memory
  type: project
---

# "Instead" defers to the attack's damage step

Aquinnah (1) `01082` / (3) `02308`: *"[reaction] When an enemy attacks you … **Deal that
enemy's damage to any enemy at your location, instead.** (You still take horror dealt by the
attack.)"*

Grimoire *Instead*: *"'instead' is indicative of a replacement effect. A replacement effect is
an effect that **replaces the resolution of a triggering condition** with an alternate means of
resolution."* The attack still happens — only **how its damage is assigned** changes. So the
redirect must resolve *with the attack*, not during the `#when EnemyAttacks` window.

Grimoire *Cancel*: *"If an effect is canceled, it is considered not to have happened."*
ArkhamDB *Cancel*: *"the ability is still regarded as initiated, and any costs have still been
paid."* So Dodge after Aquinnah zeroes **everything** — no redirected damage, no horror — but
Aquinnah stays exhausted with her 1 horror.

## The bug (2026-09-25, `~/Downloads/arkham-debug_aquinnah_dodge.json`)

`Aquinnah3.hs` used to set `attackDealDamage = False` and then `chooseDamageEnemy` **inside the
window**. Replay of that export, Daniela vs Primordial Evil `08687` (retaliate):

| step | Primordial Evil damage | window offers |
|---|---|---|
| 124 | 2 | Dodge, Aquinnah (3), Skip |
| 125 | **4** — redirect already dealt | Dodge, Skip |
| 127 | 4 | Dodge, Skip |

The window re-offering Dodge is *not* the defect — Dodge is legally playable there. The defect
was that the attack had already part-resolved, so cancelling it left damage on the board from
an attack that "did not happen."

## The shape now

- `EnemyAttackDetails` carries `attackDamageReplacement :: [Message]` (same idea as
  `attackAfter`; defaults `[]`, `.:?` in the hand-written `FromJSON`).
- Aquinnah's `UseCardAbility` only registers `[DoStep 1 msg]` and `attackDealDamage = False`;
  the `DoStep 1 (UseCardAbility …)` clause does the actual `chooseDamageEnemy`.
- `Enemy/Runner.hs`'s `PerformEnemyAttack` pushes `details.damageReplacement` **before**
  `attackMessage`, under the same `allowAttack && not details.cancelled` guard.
  `EnemyLocation/Runner.hs` has a parallel `PerformEnemyAttack` that needs the same treatment.
- Bonus: the damage amount and the legal target list are now read at resolution time, and the
  player is asked where the damage goes at the moment it is dealt.

## `updateAttackDetails`, not `changeAttackDetails`

Every caller of `changeAttackDetails` passes `getAttackDetails attrs.windows` — the copy
**frozen into the window**, which predates what `Do (EnemyAttack)` wrote (`attackCanBeCanceled`,
`attackDamageStrategy`) and anything another card in the same window wrote. A blind overwrite
therefore silently reverts them. `Arkham.Message.Lifted.updateAttackDetails` reads the live
record via `fieldMayJoin EnemyAttacking` (same lookup `isAttackCancelled` uses at
`Helpers/Window.hs:495`) and patches it, falling back to the frozen copy for the coerced
`EnemyLocation` enemy id that has no entity behind it. Aquinnah (1)/(3), Heroic Rescue and
Retribution (2) are its only callers.

Related: [[project_defeated_attacker_must_finish_its_attack]] (same card, and the reason its
"deals the redirected damage during the window" description is now historical),
[[project_divided_damage_must_batch_per_enemy]],
[[project_cannot_be_prevented_needs_both_halves]].
