---
name: project_fully_cancelled_damage_strands_its_own_reducer
description: "An `EffectDamageWindow` effect that reduces a hit to exactly 0 destroyed its own disable trigger — the `after TakeDamage` window is only opened `when (amount' > 0)` — so its `DamageTaken` modifier stuck to the target forever (#5807)"
metadata:
  node_type: memory
  type: project
---

# A reducer that cancels the whole hit used to strand itself

`damageModifier` / `reduceDamageTaken` (`Helpers/Modifiers.hs:751`,
`Message/Lifted.hs:2276`) are `createWindowModifierEffect EffectDamageWindow`, and an
`EffectDamageWindow` effect had exactly **one** disable site
(`Effect/Runner.hs`):

```haskell
Do (CheckWindows windows') | any (isTakeDamage a) windows' && isEndOfWindow a EffectDamageWindow ->
  a <$ push (DisableEffect effectId)
```

`isTakeDamage` demands an **`after TakeDamage`** window on that enemy. But the enemy's
`Damaged` handler only opens the after-windows for damage that survived the reductions
(`Enemy/Runner.hs`, the `when (amount' > 0)` guard, correct per #5682 — damage reduced
away was never dealt).

So a reducer that cancels the *entire* hit destroyed the only thing that would have
ended it. The `DamageTaken (-n)` modifier then sat on the target permanently, and every
later hit was reduced again.

**Bertie Musgrave** (`10701b`, Hemlock Vale Resident) is the card that surfaced it: his
ability 2 redirects a Resident's whole damage onto himself with
`damageModifier (attrs.ability 2) eid (DamageTaken (-n))`. Two hits at Mother Rachel
left `-2` and `-1` stranded on her; Bertie then took the 3 damage himself, died, and went
to the victory display — so the modifiers' source was no longer in play at all and Mother
Rachel could never be damaged again (#5807). The export shows the two orphaned effects in
`gameEntities.effects` with `window: {"tag":"EffectDamageWindow"}`.

Every full-cancel user of `reduceDamageTaken` had the same hole — Maria Rivera (Lost
Pilgrim), Desiderio Delgado Alvarez (Red in His Ledger), Looming Goatspawn — as did the
partial reducers (Neith, Nascent Dark Young, Fortune's Shield) against any 1-damage hit.
`reduceDamageTakenTo` (`Helpers/Enemy.hs`) was never affected: it rewrites the queued
`Damaged` message instead of creating an effect.

Fixed by also ending the effect on `AssignedDamage`, which the enemy's `Damaged` handler
pushes **unconditionally**, once per resolved assignment, with the enemy's own target.
It is the right anchor because the only modifiers `damageModifier` ever carries
(`DamageTaken`, `MaxDamageTaken`) are read solely by `getModifiedDamageAmount` during
`Damaged` — strictly *before* `AssignedDamage` — so disabling there cannot shorten a
modifier's useful life. `GraysAnatomyTheDoctorsBible5` already self-disables on
`AssignedDamage`, so the idiom was established.

It also closes the same hole for investigator-targeted damage-window effects
(`Investigator/Runner/Damage.hs` is the other `AssignedDamage` push site): `isTakeDamage`
only ever matched `EnemyTarget`, so those were never disabled by the old clause at all.
No card creates one today.

Still open, out of scope: the enemy-location damage path
(`EnemyLocation/Runner.hs`) gates its whole window cascade on `modifiedAmount > 0` and
reads modifiers *before* the when-windows, so a reducer reacting to its window cannot
take effect there in the first place. See [[project_after_dealt_damage_is_post_defeat]]
for the #5682 ordering this interacts with.
