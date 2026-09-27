---
title: project-cannotbedamaged-investigator-gap
description: CannotBeDamaged now honored for investigators (damage zeroed, horror unaffected) since the Ultimatums & Boons work; previously enemy-only
---

**Fixed 2026-07** (Boon of Osiris, [[project-ultimatums-and-boons]]): `CannotBeDamaged` on an investigator is now honored — `Investigator/Runner/Damage.hs` `handleInvestigatorAssignDamage` and `handleInvestigatorDirectDamage` zero out the damage portion when the modifier is present. Horror is deliberately unaffected ("cannot be damaged" ≠ "cannot take horror"). Enemies were always covered via `Game.hs` `EnemyCanBeDamagedBySource`.

Residual caveats:
- Horror immunity still has no generic modifier on this path — "cannot take horror" effects need their own mechanism.
- The Dawn prelude acts (10682/10684) that stack `CannotBeDamaged, CannotBeDefeated` on `Anyone` now get real damage immunity too (previously a no-op; harmless there since those preludes have no damage sources, but behavior changed from no-op to active).

**How to apply:** investigator damage immunity can now use `CannotBeDamaged` (e.g. via `nextTurnModifiers iid source target [CannotBeDamaged]`). For horror immunity, still verify/extend the path.
