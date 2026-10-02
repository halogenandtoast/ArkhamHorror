---
title: A True Magick activation is the revealed copy, not True Magick
date_added: 2026-10-02
source: internal decision (user instruction) — ratifies FAQ v2.5 Q69 + the Feb 2025 rules-forum ruling
affects:
  - True Magick (Reworking Reality) (5)
  - Sign Magick (3)
  - "Throw the Book at Them!"
  - ExcludeWindowAssetExists
---

# A True Magick activation is the revealed copy, not True Magick

**Q: You resolve an in-hand [[Spell]] through True Magick: Reworking Reality (5).
Which asset did you just "activate an [action] ability on"? Can upgraded Sign
Magick then point back at True Magick for a *different* spell?**

A: You never activate True Magick. True Magick **becomes a copy of the revealed
card** — cost, name, text box and traits (FAQ v2.5 Q69) — and the asset you
activate is that copy. So:

- Sign Magick (3) **may** point back at True Magick after a True Magick
  activation. True Magick-as-Cosmic-Flame is "a different [[Spell]] asset" from
  True Magick-as-Second-Sight, because identity follows the revealed card.
- The card just revealed is the **same** asset and must **not** be offered
  again. Identity is by card, so two copies of the same [[Spell]] in hand are
  also the same asset and both drop out.
- Sign Magick's list therefore names the **spells**, not True Magick, and
  reveals each as it is chosen — True Magick resolves them "by revealing them
  from your hand", so the reveal is owed even when the borrowed ability is
  offered directly.

Sign Magick (3) still grants an **[action]** activation only, so a borrowed
[fast]-only spell (Scrying (3)) is not a legal entry. "Throw the Book at Them!"
allows an [action] **or** [fast] ability, so it keeps the full window set.

## Affected cards / systems

- True Magick (Reworking Reality) (5) — `backend/arkham-api/library/Arkham/Asset/Assets/TrueMagickReworkingReality5.hs`
- Sign Magick (3) — `backend/arkham-api/library/Arkham/Asset/Assets/SignMagick3.hs`
- "Throw the Book at Them!" — `backend/arkham-api/library/Arkham/Event/Events/ThrowTheBookAtThem.hs`
- Engine: `ExcludeWindowAssetExists` resolves through
  `getWindowActivatedAsset`, which deliberately does **not** look through
  `ProxySource (CardIdSource _)` — `backend/arkham-api/library/Arkham/Helpers/Window.hs`

## Implementation status

✅ Implemented (#5801). `getWindowActivatedAsset` returns `Just Nothing` for a
borrowed activation, so no asset in play is excluded; Sign Magick drops the
`NonActivateAbility` wrapper and every ability whose card code was revealed in
the window (which covers both the proxy and, while the copy is still live, the
copy's own ability off `getAbilities`); `HasTrueMagick` applies the same
exclusion so the reaction is not offered with nothing left to reveal.

Do not "fix" this back into excluding True Magick from its own reaction — that
was the pre-#5801 behaviour and it made the signature Sign Magick/True Magick
combo unreachable unless a second [[Spell]] asset happened to be in play.
