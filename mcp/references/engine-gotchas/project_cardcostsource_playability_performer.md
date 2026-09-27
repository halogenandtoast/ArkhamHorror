---
title: project_cardcostsource_playability_performer
description: "During a card's playability check, ThisCard source becomes CardCostSource which has no controller, so performer-based modifiers (CannotHealDamage etc.) are not applied unless you fall back to the active investigator"
---

When a card's `cdCriteria` references its own source (`HealableInvestigator ThisCard`, `HealableAsset ThisCard`, etc.), the playability path (`Helpers/Playable.hs`) rewrites `ThisCard` → `CardCostSource (cardId)`. `getSourceController (CardCostSource _) = Nothing` (`Helpers/Source.hs`), so any **performer-based** modifier keyed on the source's controller (e.g. `CannotHealDamage`, checked via `getSourceController`/`sourcePerformerHasModifier`) is silently **not applied during the playability check** — even though it correctly applies at resolution (where the source is the in-play `EventSource`/`AssetSource` with a real controller).

Symptom: a card that should be unplayable (no valid targets because the performer is blocked) still shows as playable, then resolves to nothing.

Fix pattern (used for "Called to Guinée" + Infuse Life, issue #4847): in the matcher, when the source has no controller AND is a `CardCostSource`, fall back to `getActiveInvestigatorModifiers` for the performer modifier. The horror branches of `HealableAsset`/`HealableInvestigator` already use `getActiveInvestigatorModifiers` as the performer proxy; the damage branches did not, which was the gap.

`CannotHealDamage` is a performer restriction ("you cannot heal anything"); `CannotHaveDamageHealed` is the target restriction ("this card's damage can't be removed"). Don't confuse them.
