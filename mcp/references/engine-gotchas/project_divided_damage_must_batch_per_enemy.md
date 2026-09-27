---
name: project_divided_damage_must_batch_per_enemy
description: "Every DealDamage on an enemy opens its own WouldTakeDamage window, so 'assign N damage divided' must push ONE message per enemy carrying that enemy's total, never N one-point messages (#5665)"
metadata:
  node_type: memory
  type: project
---

`Msg.DealDamage (EnemyTarget eid) …` in `Arkham/Enemy/Runner.hs` opens a fresh
`Window.WouldTakeDamage source target amount DamageDirect` (plus the `DealtDamage` /
`TakeDamage` pairs) for **every message**. So a card that assigns damage one point at a
time raises one would-take-damage window **per point**, and any `EnemyWouldTakeDamage`
forced ability fires that many times — its default `GroupLimit PerWindow 1` does not help,
because each point is genuinely a new window.

Flamethrower (5) (`04305`) did exactly that:

```haskell
let toMsg eid' = DealDamage (EnemyTarget eid') $ delayDamage $ isDirect $ attack attrs 1
replicateM_ damage $ chooseTargetM iid engaged $ push . toMsg
```

Against **Mimetic Nemesis, Infiltrator of Realities** (`09715`) — *"Forced – When Mimetic
Nemesis would take 1 or more damage from an attack or player card: Choose and reveal a
concealed mini-card. If it is a decoy, cancel all damage just dealt"* — a single 4-damage
attack fired the Forced ability 4 times and burned 4 concealed mini-cards (#5665).
`insteadOfDamage` (`Arkham/Helpers/Enemy.hs`) only strips the `Damaged` messages already in
the queue, so cancelling the first point leaves the other three `DealDamage`s to re-trigger.

**Fix shape:** collect the whole distribution in ONE question, then push one `DealDamage`
per enemy carrying that enemy's total.

```haskell
labeled "assignAmongEngaged"
  $ chooseEnemyAmounts iid ("$" <> labelKey "assignAmongEngaged") damage engaged attrs
...
ResolveAmounts _ choices (isTarget attrs -> True) -> do
  let assignments = [(EnemyId nu.nuUUID, n) | (nu, n) <- choices, n > 0]
  for_ assignments \(eid, n) ->
    push $ DealDamage (EnemyTarget eid) $ delayDamage $ isDirect $ attack attrs n
  for_ assignments \(eid, _) -> checkDefeated attrs eid
```

`chooseEnemyAmounts` (`Arkham/Message/Lifted.hs`, next to `chooseAssetAmounts`) keys each
`AmountChoice` by the enemy's own UUID so two copies of a name stay distinct, and uses
`MaxAmountTarget` — "assign **up to** N" is the actual wording on every such card, and the
old `replicateM_` forced the player to spend all N.

Keep `delayDamage` + the explicit `checkDefeated` pass: with a 3/1 split you still want both
halves dealt before either enemy's defeat resolves.

Verified with `arkham-replay --undo 16 --answers` on the #5665 export: the forced ability
went from **4 triggers of `WouldTakeDamage … 1`** to **1 trigger of `WouldTakeDamage … 4`**.

Related: [[project_discardedfromhand_is_per_card_not_per_event]] (same class of bug on the
discard side), [[project_reaction_default_limit_is_per_player]].
