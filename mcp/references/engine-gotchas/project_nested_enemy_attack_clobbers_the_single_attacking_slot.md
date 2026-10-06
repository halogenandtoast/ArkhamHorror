---
name: project_nested_enemy_attack_clobbers_the_single_attacking_slot
description: "`enemyAttacking` is ONE slot, so a Retaliate attack provoked from inside an enemy's own `when ... attacks` window overwrote and then cleared the interrupted attack's details — its `PerformEnemyAttack` then crashed on `fromJustNote \"missing attack details\"` (#5808)"
metadata:
  node_type: memory
  type: project
---

# A nested attack by the same enemy strands the attack it interrupted

`EnemyAttrs.enemyAttacking :: Maybe EnemyAttackDetails` (`Enemy/Types/Attrs.hs:62`) is a
**single slot**, written by `Do (EnemyAttack details)` and cleared unconditionally by
`After (EnemyAttack details)` (`Enemy/Runner.hs`). Four handlers read it with
`fromJustNote "missing attack details"` — `ChangeEnemyAttackTarget`, `AfterEnemyAttack`,
`PerformEnemyAttack`, and the `CancelEachNext [AttackMessage]` case.

Two attacks by the **same** enemy can legitimately be in flight at once:

1. `Do (EnemyAttack regular)` sets the slot and queues
   `[whenWouldAttack, whenAttacks, PerformEnemyAttack, After (PerformEnemyAttack), afterEvenIfCancelled]`.
2. In the `when ... attacks` window the player triggers **Survival Knife** (53002)
   ability 2 — "*before resolving that attack*, exhaust: Fight. This attack targets the
   attacking enemy." The fight runs nested inside that window.
3. The fight fails, the enemy has **Retaliate**, so `Failed (Action.Fight, …)`
   (`Enemy/Runner.hs:1378`) pushes a second `EnemyAttack`. Its `Do` **overwrites** the
   slot, its `After` then sets it to `Nothing`.
4. The queue resumes the first attack's `PerformEnemyAttack` → `fromJustNote` on
   `Nothing` → a 500 the player sees as "the game is stuck".

Reported as #5808 against Hunting Horror (02141, Hunter + Retaliate) in The Miskatonic
Museum. The issue title blames the horror assignment ("can't put horror on anyone but
Adam Lynch"); it is a red herring — every choice in that prompt crashes identically,
because the crash is on the message *after* the prompt. Per the rules both attacks must
resolve: the Retaliate attack first, then the original.

Fixed by having `Do (EnemyAttack details)` append
`ChangeEnemyAttackDetails enemyId outer` (the existing message, `Message/EnemyAttack.hs:36`,
already handled as `attackingL ?~ details'`) to the block it pushes whenever the slot is
already occupied. `pushAll` prepends in order, so the reinstate lands after the nested
attack's whole chain and before the interrupted attack's `PerformEnemyAttack`. No new
message, no change to the persisted `EnemyAttrs` shape.

**The massive-attack path must be excluded.** `PerformEnemyAttack`'s
`MassiveAttackTargets` branch queues one `EnemyAttack` per target *after* the parent has
performed, and never pushes an `After (EnemyAttack)` of its own — today the last
sub-attack's `After` is what leaves the slot `Nothing`. Reinstating the parent there would
leave `attacking` set forever, which matters:
`Entities.clearRemovedEntities` keeps a `RemovedZone` enemy in the map while the slot is
set, and the `PerformEnemyAttack` guard (`not enemyDefeated || isJust enemyAttacking`,
from "Let a defeated attacker finish the attack it had begun") lets a defeated enemy
attack while it is set. Hence the `delegatedFrom` test: skip the reinstate when the
occupant is a `MassiveAttackTargets` attack and the new details' target is one of its
targets.

`filterOutEnemyMessages` also drops the reinstate now, so an enemy that leaves play
mid-attack (Hunting Horror's own void ability, `HuntingHorror.hs:47`) cannot have details
restored for an enemy that is gone.

**Verification note:** replaying the #5808 export *without* `--undo` crashes even with the
fix, and that is expected — the export was captured after the clobber, so its saved queue
holds a bare `PerformEnemyAttack_` (idx 17 at step 743) with no `Do (EnemyAttack)` and no
reinstate ahead of it. No code change can rescue that queue; the same is true of the live
game, which has to be rewound with `PUT /undo` to re-run the attack under fixed code.

To exercise the fix, rewind to **step 735**, whose queue is `[Do EnemiesAttack_,
RelentlessEnemiesAttack_, PhaseStep, PhaseStep]` — i.e. before the attack begins, so the
whole nesting is regenerated:

```bash
# 743 - 8 = 735; then re-answer: pick the enemy, trigger Survival Knife, fight, start test,
# apply results -> Retaliate nests a second attack inside the first's `when attacks` window
stack exec arkham-replay -- export.json --undo 8 --answers answers.json --output after.json
```

The run lands on the same "Assign 1 horror" prompt at step 233 the user reported, and
answering it now drains cleanly: investigator `04001` goes Damage 2 -> 4 and Horror 0 -> 2
(both 1/1 attacks resolve), with the enemy ending `attacking = null`. Pre-fix, that same
answer died on `fromJustNote` at `Enemy/Runner.hs:1631`.

See [[project_after_dealt_damage_is_post_defeat]] and
[[project_fully_cancelled_damage_strands_its_own_reducer]] for the other two ways an
attack's own bookkeeping outlives or undercuts itself.
