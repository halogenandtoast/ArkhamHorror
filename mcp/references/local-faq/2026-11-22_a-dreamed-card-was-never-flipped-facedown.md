---
title: A card dreamed from hand was never "flipped facedown"
date_added: 2026-11-22
source: internal decision (project owner ruling)
affects:
  - dreaming
  - Staff of Atal
  - Oathsworn
  - Luke Robinson
  - Explorer's Sextant
  - Lash of Ulthar
  - Dream Quest
---

# A card dreamed from hand was never "flipped facedown"

Playing a card with the dreaming keyword puts it into the dream zone **facedown**. It was
never faceup, so nothing flipped. A trigger that reads *"after an asset you control is
flipped facedown"* therefore does **not** fire when a dreaming card is played from hand.

It fires only on an effect that turns an **in-play** asset facedown:

- Oathsworn (14011)
- Luke Robinson (14045)
- Explorer's Sextant (14057)
- Lash of Ulthar (14067)
- Dream Quest (14060), as its additional cost

The corollary matters: such a flip makes the asset **leave play** (damage, uses and
attachments go with it, the slot frees), which is why it is a distinct event from a card
going down facedown out of hand, where nothing was in play to leave.

## Affected cards / systems

- Staff of Atal (14070) — `backend/arkham-api/library/Arkham/Asset/Assets/StaffOfAtal3.hs`
- the window — `Window.AssetFlippedFacedown` in
  `backend/arkham-api/library/Arkham/Window.hs`, matched by
  `Matcher.AssetFlippedFacedown` in `backend/arkham-api/library/Arkham/Matcher/Window.hs`
- the only raiser — `FlipAssetFacedown` in
  `backend/arkham-api/library/Arkham/Game/Runner.hs`
- the other, non-flip entry into the dream zone — `PlaceDreamingCard` in
  `backend/arkham-api/library/Arkham/Investigator/Runner.hs`

## Implementation status

- **the window**: ✅ already matched the ruling, structurally.
  `Window.AssetFlippedFacedown` is raised in exactly one place, `FlipAssetFacedown`, and
  only after `RemoveFromPlay` — i.e. only for an asset that really was in play. The
  from-hand path reaches `PlaceDreamingCard` directly and raises no flip window, so a
  dreamed card cannot trigger it.
- **Staff of Atal (14070)**: ✅ its printed `[reaction]` hangs on
  `AssetFlippedFacedown #after You (AssetControlledBy You)` and so only sees the five cards
  above. The `Who` is read off the window's own payload (the investigator whose dream zone
  receives the card, i.e. the asset's controller), never re-derived from an asset that has
  already left play.
- **why not `Window.LeavePlay`**: ✅ deliberately not reused — it carries no reason, so
  Staff of Atal would have fired on every asset discard.
