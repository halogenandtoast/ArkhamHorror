---
name: project_after_dealt_damage_is_post_defeat
description: "`after DealtDamage` on an enemy fires AFTER the damage lands and the enemy is defeated, so its board reads see the corpse gone — and its matcher must use the removed-tolerant `enemyMatches` or the trigger vanishes (#5682)"
metadata:
  node_type: memory
  type: project
---

# `after DealtDamage` on an enemy is a **post-defeat** window

Two rules compose and settle every "after you deal damage to an enemy" question:

1. **Defeat happens *inside* the dealing of damage.** Rules Reference / Grimoire,
   *Dealing Damage/Horror*, step 2 (Apply): "*…After applying damage/horror, if an enemy has
   damage equal to or higher than its health, it is defeated and placed in the encounter
   discard pile.*"
2. **"After…" effects execute only once the triggering condition has fully resolved.**
   FAQ v2.5 §1.4 / Grimoire *Nested Sequences*. The Guard Dog / Goat Spawn worked example is
   explicit: 1 damage "*which would defeat it*" resolves the defeat and its own windows first,
   "***Then**, the players resolve the damage dealt to the Guard Dog, and resolve any 'After…'
   effects that might occur from that damage.*"

So a lethal hit **still fires** the trigger (the damage *was* dealt — nothing cancels it), but
the trigger resolves with the enemy already defeated, discarded, and off its location. Effects
that target the dead enemy or read its location simply fizzle.

There is **no card-specific ruling** anywhere for this — ArkhamDB's rulings box is empty for
`84003`, `10693b`, `88049b`, `07330b`/`07331b`, `81036`, `12003`; `03067b`/`07330a` carry only
unrelated errata; no FFG/designer statement exists on the forum archive, BGG, Mythos Busters,
or Hall of Arkham. The rules above are the whole answer.

## Where the order lives — three separate cascades

`Enemy/Runner.hs` (`Msg.DealDamage (EnemyTarget …)`) and `Behavior/Damage.hs:fireDamageWindows`
(used by `EnemyLocation/Runner.hs`, `KeyLocusDefensiveBarrier`, `TheHeartOfMadness/Pylon`) both
run:

```
when WouldTakeDamage → when DealtDamage → when TakeDamage
  → Damaged   (pushes AssignedDamage + checkDefeated ahead of what follows)
  → after DealtDamage → after TakeDamage
```

`after DealtDamage` used to sit **above** `Damaged`, which is what let Special Investigation
(`84003`) make a lethally-wounded Arkham Officer attack before dying (#5682).

**The investigator (`Investigator/Runner/Damage.hs`) and asset (`Asset/Runner.hs`) cascades
still have the old pre-application order.** They serve a different card set — Survival Knife,
Pit Viper, Serpent of Tenochtitlan, Dweller in the Pit, Sparrow Mask, Hunter's Armor, Twisting
Catwalks — each of which needs its own ruling check before moving. Know which target type your
window carries before reasoning about its timing.

## The matcher has to read the corpse back

`Helpers/Window.hs`'s `EnemyDealtDamage` / `EnemyDealtExcessDamage` originally did
`elem eid <$> select enemyMatcher`, which cannot see an enemy that has just left play — so
once the window moved post-defeat, a killing blow would have silently dropped the trigger
(Dimensional Duplicator has **1 health**; Ishimaru Haruko, Zoey Samaras (Parallel), In Harm's
Way all depend on it firing). They now use `Arkham.Helpers.Window.Enemy.enemyMatches`
(`orM [matches eid m, matches eid (OutOfPlayEnemy RemovedZone m)]`) — the same read-back the
evade windows use ([[project_evade_windows_name_removed_enemies]]).

Note the two `enemyMatches` are different functions: `Helpers/Enemy.hs`'s is a plain `select`,
`Helpers/Window/Enemy.hs`'s is the removed-tolerant one. `Helpers/Window.hs` imports only the
latter, unqualified.

**Consequence for card matchers:** a clause nested *under* the damaged enemy (`EnemyAt …`,
`locationWithEnemy a`) will not hold once that enemy is dead, because the `RemovedZone` arm has
no placement. Special Investigation had its "at a location with a ready Police enemy" clause
nested under `EnemyAt` on the Humanoid; it now hangs off `SourceUsedBy (You <> at_ …)` — which
is both what the card prints ("*an investigator at the same location as a ready Police
enemy*") and what keeps the second-Officer-still-standing case working.

Mother Rachel (`10693b`) deliberately lapses under the same rule: "*choose a different Resident
enemy **at her location***" has no location to read once she is defeated.

Related: [[project_leave_play_tombstones]], [[project_ifenemydefeated_resolves_after_disposal]],
[[project_divided_damage_must_batch_per_enemy]],
[[project_enemy_location_runner_must_mirror_enemy_windows]].
