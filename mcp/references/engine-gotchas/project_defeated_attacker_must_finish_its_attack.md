---
name: project_defeated_attacker_must_finish_its_attack
description: "An attack already in flight still resolves when a `#when EnemyAttacks` interrupt defeats the attacker — Aquinnah (3) redirecting damage onto the attacking enemy lost the attack's horror entirely, because the enemy was pruned by `clearRemovedEntities` before `PerformEnemyAttack` and the handler's `not enemyDefeated` guard skipped it"
metadata:
  node_type: memory
  type: project
---

# A defeated attacker still finishes the attack it had already begun

**Defeat does not cancel an attack.** FAQ v2.5 §1.4 / Grimoire *Nested Sequences*: a `#when`
interrupt spawns a nested sequence that resolves completely — including the enemy's defeat and
disposal — and *then* "*the game returns to where it left off, continuing with the original
triggering condition's sequence*". The original triggering condition here is the attack, so it
resolves against a target whose attacker is already in the victory display.

Aquinnah (3) (`02308`) prints the conclusion outright: "*Deal that enemy's damage to any enemy
at your location, instead. (**You still take horror dealt by the attack.**)*" The (3) upgrade
changes "another enemy" to "**any** enemy" precisely so you may dump the damage onto the
attacker — the horror is the price, and it does not evaporate when that kills it.

## Why the horror vanished

Three things compounded, and only the third is obvious from the card code:

1. `Aquinnah3.hs` set `attackDealDamage = False` and dealt the redirected damage during the
   `#when EnemyAttacks` window. `PerformEnemyAttack` reads `healthDamage = 0` but
   `sanityDamage = field EnemySanityDamage` — the horror was never the cancelled part.
   (As of 2026-09-25 the redirect is deferred into `PerformEnemyAttack` itself via
   `attackDamageReplacement` — see [[project_aquinnah_redirect_is_a_damage_replacement]] — so
   the attacker is now defeated *inside* the attack rather than before it. The invariants
   below still stand for every other `#when` interrupt that can kill an attacker.)
2. The redirected damage defeats the attacker → `AddToVictory` → `RemoveEnemy`, which only
   sets `enemyPlacement = OutOfPlay RemovedZone`. **`ResolvedAbility` then runs
   `clearRemovedEntities` (`Game/Runner.hs`), which deletes the entity outright — and that
   fires before `PerformEnemyAttack`.** The message is processed against nothing and pushes
   no follow-up at all.
3. Even with the entity alive, `PerformEnemyAttack`'s `&& not enemyDefeated` guard
   (`e8e79498cc`, "Defeated enemies should not continue attack") skipped it.

Two unrelated hooks also fell through the same hole: `Investigator/Runner.hs`'s
`PerformEnemyAttack` → `HistoryEnemiesAttackedBy` (Daniela Reyes' elder sign reads it) and
`DanielaReyes.hs`'s own `PerformEnemyAttack` meta.

## The invariant now

- `EnemyAttack` / `Do (EnemyAttack)` are gated on `not enemyDefeated` — a defeated enemy never
  **starts** an attack. That is what the 2024 guard was really protecting.
- `PerformEnemyAttack` is gated on `not enemyDefeated || isJust enemyAttacking` — an attack
  already **begun** resolves regardless.
- `attackingL` is cleared in `After (EnemyAttack details)`. It was previously *never* cleared,
  so `isJust enemyAttacking` now actually means "mid-attack". Watch for this: it also un-breaks
  the `AttackingEnemy` / `NotAttackingEnemy` matchers (`Game.hs`'s `AttackingEnemy` is literally
  `fieldMap EnemyAttacking isJust`), which had been permanently true for any enemy that had
  ever attacked — Oculus Mortuum and Bishop's Brook (`02202`) depend on them.
- `clearRemovedEntities` keeps an enemy whose `enemyAttacking` is set, so it survives its own
  attack. It is pruned at the next `ResolvedAbility`/`BeginTurn` once the attack's After step
  clears the field.
- The `Exhaust` push in `PerformEnemyAttack` is skipped for a defeated attacker.

## Reproducing

`~/Downloads/arkham-debug_aquinnah.json` (Daniela, Night of the Zealot): `arkham-replay --undo 1`
parks on the Aquinnah (3) reaction to a Whippoorwill-class AoO (`01138`, 1/1). Answer it and
the trace should show `InvestigatorAssignDamage_ "08001" (EnemyAttackSource …) DamageAny 0 1`
right after `PerformEnemyAttack_`; before the fix `PerformEnemyAttack_` produced nothing.

Related: [[project_after_dealt_damage_is_post_defeat]] (the sibling case where the window fires
but reads a corpse), [[project_addtovictory_leaveplay_window_ordering]],
[[project_ifenemydefeated_resolves_after_disposal]].
