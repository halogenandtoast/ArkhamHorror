---
title: project_deal_damage_is_enemies_only
description: DealDamage has exactly one handler, for EnemyTarget — aimed at an investigator or an asset it reaches nothing and the damage silently never happens
---

`Msg.DealDamage target assignment` looks like the general way to deal damage.
It is not: across the whole engine the only arm for it is

```haskell
Msg.DealDamage (EnemyTarget eid) assignment | n > 0 -> …
```

so one pointed at an investigator or an asset falls through every runner and the
damage never happens. No error, nothing in the log.

Each other entity has its own message, and an investigator has **two**, which
is the distinction the card text makes:

| what the card says | message |
| --- | --- |
| "take 1 damage" (where the card says) | `InvestigatorMessage (InvestigatorDirectDamage_ iid source d h)` |
| "deal 1 damage to each investigator" (they assign it) | `InvestigatorMessage (InvestigatorAssignDamage_ iid source DamageAny d h)` |
| damage onto an asset | `DealAssetDamageWithCheck aid source d h True` |

Note the asset message carries damage *and* horror at once, while `DealDamage`'s
assignment record has no horror field at all -- horror on an asset or an
investigator has to go through one of the above.

Two custom cards did this: Tarun Chandavarkar's elder sign ("deal 1 damage or
horror to a card you control") and Uncontrolled Fire ("deal 1 damage to each
investigator at your location"). Both read as finished cards and both halves
were dead. The custom-card checker now rejects a `DealDamage_` whose target is
not an enemy.
