---
name: project_hollow_leaks_past_leaveplay_replacement
description: "hollow helper stamps its Hollow/hollowed tag effects OUTSIDE the leave-play window batch, so a would-leave-play replacement (cancelWindowBatch) does not suppress the tag"
metadata: 
  node_type: memory
  type: project
  originSessionId: 234c52e7-9b18-46d9-b367-df0b6b4979c6
  modified: 2026-07-24T02:56:08.210Z
---

The Scarlet Keys `hollow iid card` (`Arkham/Campaigns/TheScarletKeys/Helpers.hs`) does
`setCardAside` (→ `obtainCard` → `ObtainCard` → `RemoveFromPlay` → `Window.LeavePlay`) and THEN
pushes, as **separate** messages, two `createWindowModifierEffect_ (EffectHollowWindow card.id)`
(the `Hollow card.id` tag on the investigator + `CampaignModifier "hollowed"` on the card) plus a
`CampaignEvent "hollowed"` window. `HollowedCard`/`CardWithHollowedCopy` match only on the
`CampaignModifier "hollowed"` tag.

**Trap:** a forced "when it would leave play, instead set aside out of play" replacement (e.g. Dancing
Mad Act 1 **False Step v.I**, `forced (AssetWouldLeavePlay #when …)`) runs `cancelWindowBatch ws`,
which cancels ONLY the `RemoveFromPlay` batch. The hollow's tag-stamping effects are not in that
batch, so they still apply — the card ends up set-aside **and** `hollowed`. This is how the
Desiderio ally wrongly landed in the hollowed cards (issue #5234; that hollowed enemy-title copy then
mis-fired Otherworldly Mimic).

Confirmed from the full `dm2.json` export: at the hollow step the intercept DID fire (horror dealt,
ability in `usedAbilities`) yet the `hollowed` `CreateWindowModifierEffect` on Desi's card still
executed.

**Fix pattern (localized):** in the replacing ability's handler, after `cancelWindowBatch`, use
`allMatchingDon't` to drop the queued hollow follow-ups keyed to the leaving card —
`CreateWindowModifierEffect (EffectHollowWindow cid) _ _ _` (cid==card.id, catches both tags) and
any `CheckWindows`/`Do (CheckWindows)` carrying a `Window.CampaignEvent "hollowed"` — then do the
plain `setCardAside`. Mirrors the `PlaceInBonded` precedent (`Arkham/Asset/Runner.hs`, strips queued
discard/window msgs) and `TheGoldPocketWatch4`. A general fix (batch hollow's tagging under the
leave-play window) is more principled but touches shared hollow infra. Related:
[[project_enemy_removal_attached_treacheries]].
