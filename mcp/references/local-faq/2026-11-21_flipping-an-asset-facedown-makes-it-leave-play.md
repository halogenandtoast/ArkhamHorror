---
title: Flipping an in-play asset facedown makes it leave play
date_added: 2026-11-21
source: internal decision (project owner)
affects:
  - dreaming
  - Power of the Dreamlands
  - Oathsworn
  - Luke Robinson
  - Explorer's Sextant
  - Lash of Ulthar
  - Dream Quest
---

# Flipping an in-play asset facedown makes it leave play

An asset flipped facedown into a dream zone **leaves play**. Damage and uses on it are
removed, anything attached to it is discarded, and its slot is freed. When the last dream
comes off, it **enters play fresh**, with its full printed uses.

## Affected cards / systems

- `FlipAssetFacedown InvestigatorId AssetId Int` —
  `backend/arkham-api/library/Arkham/Game/Runner.hs`
- `RemoveFromPlay` — `backend/arkham-api/library/Arkham/Asset/Runner.hs`
- `FlipDreamingCardFaceup` / `PutCardIntoPlay` —
  `backend/arkham-api/library/Arkham/Investigator/Runner.hs`,
  `backend/arkham-api/library/Arkham/Game/Runner.hs`
- `Window.AssetFlippedFacedown InvestigatorId AssetId` —
  `backend/arkham-api/library/Arkham/Window.hs`

## Implementation status

- **leaving play**: ✅ `FlipAssetFacedown` pushes `RemoveFromPlay (AssetSource aid)` and
  nothing else teardown-shaped. `RemoveFromPlay` already raises the leave-play windows at
  `#when`/`#atIf`/`#after`, unseals sealed chaos tokens, discards attached assets and
  events, discards cards underneath, and drops the entity (so damage and uses go with it);
  `RefillSlots` frees the slot. **Do not hand-roll any of this** — a parallel teardown is
  how a flip silently keeps damage or a sealed token.
- **entering play fresh**: ✅ comes for free. The flip faceup routes through
  `PutCardIntoPlay`, which builds a new entity from the card def, so printed uses are full.
- **the flip window**: ✅ `Window.AssetFlippedFacedown` carries the investigator, and the
  matcher checks that payload rather than running an investigator matcher over the asset
  (#5685 / #6013). `Window.LeavePlay` is NOT usable for this: it carries no reason and
  would fire on every asset discard. The asset id is also registered in
  `leavePlayWindowAssets` (`Arkham/Game.hs`), without which a reaction reading the asset
  half throws `MissingEntity`.
- **re-charging**: ✅ `createActiveCostForAdditionalCardCosts`
  (`Arkham/Game/Utils.hs`) deliberately does NOT add a dream cost. The flip faceup goes
  through `PutCardIntoPlay`, so a dream cost there would re-charge the dreams on every
  flip — infinite regress.
