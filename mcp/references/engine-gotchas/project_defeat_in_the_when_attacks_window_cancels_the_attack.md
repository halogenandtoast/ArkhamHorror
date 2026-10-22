---
name: project_defeat_in_the_when_attacks_window_cancels_the_attack
description: "`enemyAttacking` is set BEFORE the `#when EnemyAttacks` window, so it cannot mean \"the attack has begun resolving\" — a guard built on it let an attacker killed by Survival Knife (2)'s interrupt deal its damage anyway (#5890)"
metadata:
  node_type: memory
  type: project
---

# An attacker defeated in the when-attacks window must not attack

`Do (EnemyAttack details)` (`Enemy/Runner.hs`) writes `attackingL ?~ details` and pushes

```
[whenWouldAttack, whenAttacks, PerformEnemyAttack, After (PerformEnemyAttack), afterEvenIfCancelled]
```

in that order. **The slot is filled before the when-window runs**, so `isJust
enemyAttacking` is true throughout the interrupt and says nothing about whether the attack
has started resolving. The only thing that distinguishes the two cases is *where* the
defeat landed: before `PerformEnemyAttack`, or inside it.

Two cards sit on opposite sides of that line and both are printed to work:

| card | window | what kills the attacker | must the attack resolve? |
| --- | --- | --- | --- |
| Survival Knife (2) (53002) | `EnemyAttacks #when` | the reaction's own Fight, fully resolved inside the window | **no** — the card prints "*(before resolving that attack)*" |
| Aquinnah (1)/(3) (01082 / 02308) | `EnemyAttacks #when` | the redirect, which is registered as `attackDamageReplacement` and pushed from inside `PerformEnemyAttack` | **yes** — "*(You still take horror dealt by the attack.)*" |

So the guard is plain `not enemyDefeated`, with a defeated attacker handled by its own
branch that clears the slot:

```haskell
PerformEnemyAttack eid | eid == enemyId && enemyDefeated -> do
  pure $ a & attackingL .~ Nothing
PerformEnemyAttack eid | eid == enemyId -> do
  let details = fromJustNote "missing attack details" enemyAttacking
```

**Clearing `attacking` is load-bearing, not tidiness.** `Entities.clearRemovedEntities`
keeps a `RemovedZone` enemy in the map while the slot is set, so skipping the attack
without clearing it leaves the corpse on the board and matching `AttackingEnemy` forever.
Conversely the keep-alive is still needed for the Aquinnah case, where the enemy dies
*during* `PerformEnemyAttack` and `After (EnemyAttack details)` must still reach its
handler to clear the slot and open the after-attack window.

What a skipped attack does and does not raise:

- `Window.EnemyAttacks #after` does **not** fire — it is pushed from inside
  `PerformEnemyAttack`. Same as a Dodge, whose `cancelAttack` strips the whole
  `PerformEnemyAttack` out of the queue via `CancelNext (… AttackMessage)`.
- `#after EnemyAttacksEvenIfCancelled` **does** fire: `Do (EnemyAttack)` queues it
  separately. That is the window's reason to exist.
- `After (PerformEnemyAttack enemyId)` still fires (also queued by `Do`), so `PerAttack`
  ability limits still clear and `EffectAttackWindow` effects still end.
- `details.attackAfter` follow-ups (`afterEnemyAttack`, 4 callers: `GetBehindMe`,
  `HeroicRescue2`, `SethBishopThrallOfYogSothoth`, `BloodMoney`) are dropped — each is
  "after this attack, do X to the attacker", moot once it is dead.
- `Investigator/Runner.hs`'s own `PerformEnemyAttack eid` clause still records
  `HistoryEnemiesAttackedBy`, since it is a separate entity's handler. Left as is.

## History

`30f1d94aa4` ("Let a defeated attacker finish the attack it had begun", 2026-09-22) added
`|| isJust enemyAttacking` because Aquinnah was then dealing its redirected damage
*immediately inside the when-window*, so the attacker really was dead before
`PerformEnemyAttack`. Three days later `429f0c8594` ("Resolve Aquinnah's damage redirect
with the attack, not in the window") moved the redirect to `attackDamageReplacement` and
made the disjunct unnecessary, but nobody tightened it back — and it silently covered
Survival Knife (2) as well. Reported as #5890 in Spanish ("*después de derrotar a un
enemigo … no debería recibir el daño*"), against Wizard of the Order (01170, 2 health,
1 damage) in The Gathering.

## Verifying it from an export

`--undo` lands squarely on the reaction window, so this one reproduces cleanly. For the
#5890 export (current step 296):

```bash
# undo 6 -> the knife's reaction WindowChooseOne, knife unexhausted, enemy alive
# answers: trigger knife, pick enemy, skip triggers, commit a combat icon (to pass),
#          start test, skip triggers, apply results
stack exec arkham-replay -- export.json --undo 6 --answers answers.json --output after.json
```

The chaos-token stream is re-seeded from the undone state, so the live game's draw does
**not** come back — the fight failed on the first attempt. Commit a card with combat icons
to force the success instead of hunting for the original token; any change to the answer
list shifts the draw again, and a committed `Daring` (06111) will pull a card and can land
a weakness mid-sequence, so prefer an asset with combat icons.

Pre-fix the run ends on `QuestionWithSource (EnemyAttackSource …)` /
`QuestionLabel "Assign 1 damage"` with the enemy at `defeated: true`, `OutOfPlay
RemovedZone`, `attacking` still set. Post-fix that prompt is gone and `attacking` is
`null`.

See [[project_nested_enemy_attack_clobbers_the_single_attacking_slot]] for the other way
that one slot bites, and [[project_after_dealt_damage_is_post_defeat]] for the window that
fires *after* the corpse is gone.
