---
title: The dreaming card control limit is a hard cap
date_added: 2026-11-21
source: internal decision (project owner)
affects:
  - dreaming
  - Power of the Dreamlands
  - Staff of Atal
  - Oathsworn
  - Luke Robinson
  - Explorer's Sextant
  - Lash of Ulthar
  - Dream Quest
---

# The dreaming card control limit is a hard cap

"You can control up to 3 at a time" is a **hard cap**, default 3 and raisable by card
effects. At the cap:

- playing a dreaming card from hand is **unplayable** — it is not offered, and
- a forced "flip facedown" effect **does nothing**: no dreaming card is created, and the
  asset does not leave play.

It gates exactly the way a slot limit gates.

## Affected cards / systems

- the cap formula — `getDreamingCardLimit` / `canCreateDreamingCard` in
  `backend/arkham-api/library/Arkham/Helpers/Dreaming.hs`
- raising the cap — `AdditionalDreamingCard Int` in
  `backend/arkham-api/library/Arkham/Modifier.hs` (mirrors `AdditionalSlot`)
- playability — `backend/arkham-api/library/Arkham/Helpers/Playable.hs` (the dreaming
  disjunct beside `canAffordAlternateResourceCost`) and the `DreamCost` case of
  `getCanAffordCost_` in `backend/arkham-api/library/Arkham/Helpers/Cost.hs`
- forced flip does nothing — `PlaceDreamingCard` in
  `backend/arkham-api/library/Arkham/Investigator/Runner.hs`, and `FlipAssetFacedown` in
  `backend/arkham-api/library/Arkham/Game/Runner.hs`

## Implementation status

- **the formula**: ✅ the literal `3` and the `AdditionalDreamingCard` fold appear in
  exactly one function, `getDreamingCardLimit`. There is deliberately no `Field` for the
  cap; the per-investigator wire payload carries the resolved value as
  `dreamingCardLimit`, injected beside `handSize`, so the frontend never re-derives it.
- **unplayable at the cap**: ✅ two callers. `getCanAffordCost_`'s `DreamCost` case, and
  the `Helpers/Playable.hs` disjunct — the latter is load-bearing, because that path gates
  playability *without* calling `getCostForCard`.
- **a forced flip does nothing**: ✅ `PlaceDreamingCard` no-ops at the cap, and
  `FlipAssetFacedown` is gated on the cap *before* `RemoveFromPlay`, so an asset is never
  destroyed by a flip that cannot land.
- **narrated, not silent**: ✅ a refused `PlaceDreamingCard` emits a `notice`
  (`log.cannotDreamAnotherCard`) from `Arkham/Log/Narrator.hs`. A card that visibly does
  nothing with no log line is filed as a bug (#5998, #6017).
