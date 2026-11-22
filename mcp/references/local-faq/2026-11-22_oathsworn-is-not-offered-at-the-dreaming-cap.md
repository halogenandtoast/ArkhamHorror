---
title: Oathsworn's reaction is not offered at the dreaming-card cap
date_added: 2026-11-22
source: internal decision (project owner ruling)
affects:
  - dreaming
  - Oathsworn
---

# Oathsworn's reaction is not offered at the dreaming-card cap

Oathsworn (14011) prints: *"[reaction] When attached asset would be defeated: Discard
Oathsworn instead. Flip attached asset facedown and place 1 resource (from the token pool)
on it, as a dream."*

The flip is **essential to the effect**. With no room in the dream zone — the controller of
the attached asset already controls their limit of dreaming cards — the ability is
**unavailable**: it is not offered at all. The attached asset is defeated normally and
Oathsworn is discarded with it.

There is no partial resolution. Oathsworn must never replace the defeat and then fail to
flip, which would leave the asset in play carrying the damage that defeated it.

The zone that must have room is the **attached asset's controller's**, who is not
necessarily the investigator who played Oathsworn — it attaches to an Ally asset
controlled by *an investigator at your location*. The dreaming card that results is that
investigator's.

## Affected cards / systems

- Oathsworn (14011) — `backend/arkham-api/library/Arkham/Event/Events/Oathsworn3.hs`,
  def in `backend/arkham-api/library/Arkham/Event/Cards/PowerOfTheDreamlands.hs`
- the cap criterion — `Criteria.CanCreateDreamingCard InvestigatorMatcher` in
  `backend/arkham-api/library/Arkham/Criteria.hs`, resolved in
  `backend/arkham-api/library/Arkham/Helpers/Criteria.hs`
- the cap formula — `canCreateDreamingCard` in
  `backend/arkham-api/library/Arkham/Helpers/Dreaming.hs`

## Implementation status

- **Oathsworn (14011)**: ✏️ implemented. The printed `[reaction]` is ability 1 on
  `AssetDefeated #when ByAny`, restricted by
  `CanCreateDreamingCard (ControlsAsset (AssetWithId aid))` — a new `Criterion` that routes
  through the single cap formula rather than re-deriving the limit. The matcher names
  *whose* zone, which is why it takes an `InvestigatorMatcher`: the ability's user is
  Oathsworn's owner, but the zone belongs to the asset's controller.
- **no partial resolution**: ✏️ the handler re-reads `canCreateDreamingCard` for that
  controller before it does anything, so the defeat is cancelled, Oathsworn discarded and
  the flip pushed as one all-or-nothing step. (`FlipAssetFacedown` is itself gated on the
  cap and would silently do nothing, which is exactly the forbidden state.)
- **replacing the defeat**: ✏️ *"Discard Oathsworn instead"* removes the rest of the defeat
  frame — the `When`/`Do`/`After (AssetDefeated ..)` messages and the `#after` defeat
  window — so a card reading *"after an asset is defeated"* does not fire off a defeat that
  did not happen. Done with `removeAllMessagesMatchingNested`, because a defeat reached
  through a `Run` block keeps the frame out of a flat filter's reach.
- **the play itself**: ✏️ the def gained
  `cdCriteria = Just $ exists $ #ally <> at_ YourLocation <> AssetControlledBy Anyone`,
  mirroring Trusted (04019): without it the event is playable with nothing to attach to and
  silently does nothing.
