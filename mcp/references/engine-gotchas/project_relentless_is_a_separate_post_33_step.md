---
name: project_relentless_is_a_separate_post_33_step
description: Relentless (TDC) is a ready-and-attack-again step after enemy-phase 3.3, not a second attack queued alongside the first (#5589)
metadata:
  type: project
---

Relentless rules text: "During the enemy phase (after framework step 3.3), each enemy with the
relentless keyword that has attacked this phase (even if that attack was canceled) readies and
attacks the investigator(s) it is engaged with a second time."

It was originally modelled in `Enemy/Runner.hs` as `attackCount = 2` — two `EnemyWillAttack`
pushed together with only the last carrying `attackExhaustsEnemy = True`. Both landed in one
`EnemyAttacks` batch as two identical `TargetLabel`s, so correctness depended on the
non-exhausting one resolving first. It didn't: `Game/Runner.hs`'s `EnemyAttacks` handler
*prepends* a peeked `EnemyWillAttack` (`EnemyAttack details2 : as`) even though it comes after
`as` in the queue, so a second enemy attacking in the same phase flipped the pair. The frontend
picks the first choice matching the clicked enemy (`Enemy.vue`, `findIndex`), so the exhausting
attack ran first, the enemy exhausted, and `attackIsValid`'s `readyEnough` (`Arkham/Attack.hs`)
silently dropped the survivor.

Now (2026-09-03, #5589): `runEnemyPhase` runs `[EnemiesAttack, RelentlessEnemiesAttack]` in
`ResolveAttacksStep`; `Scenario/Runner.hs` handles `RelentlessEnemiesAttack` by selecting
`EnemyWithKeyword Relentless` intersected with `getAllHistoryField PhaseHistory
HistoryEnemiesAttackedBy`, then per enemy pushing `Ready` (only if exhausted) +
`ForTarget (EnemyTarget eid) (Do EnemiesAttack)`. Enemy phase attacks are back to a single
`attackExhaustsEnemy = True` attack.

**How to apply:** phase history is the "attacked this phase" source — it's recorded on
`PerformEnemyAttack` in `Investigator/Runner.hs`, which still runs for a cancelled attack, which
is what "even if that attack was canceled" needs. Because the whole enemy phase is queued up
front by `runEnemyPhase`, an `arkham-replay --undo` that lands *inside* the enemy phase replays
the OLD queue and will not contain a newly added phase step — rewind to before the phase began
(here `--undo 12`, into the investigation phase) to exercise it.
Related: [[project_attackisvalid_is_the_direct_enemyattack_gate]], [[project_drowned_city]].
