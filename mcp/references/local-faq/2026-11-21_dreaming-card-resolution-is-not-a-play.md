---
title: A dreaming card's flip-faceup resolution is not a play
date_added: 2026-11-21
source: engine behaviour (Game/Runner.hs) + printed text of 14026
affects:
  - dreaming
  - Power of the Dreamlands
  - Light of Ilarnek (14026)
---

# A dreaming card's flip-faceup resolution is not a play

A card played facedown with the **dreaming** keyword is *played* at that moment. When its
last dream comes off it **resolves** — it is not played a second time. Any restriction that
constrains the **play** therefore applies only to going facedown, and is **not re-checked**
at the flip.

Light of Ilarnek (14026) is the clean case: "Dreaming 2. Fast. **Play only during your
turn.**" You must be in your own turn to put it facedown. The clue discovery and the attach
happen later, when the last dream comes off, and nothing re-tests `cdFastWindow` or
`cdCriteria` at that point.

## Why (engine)

- `Game/Runner.hs`'s `PlayCard` short-circuits for a dreamed card:
  `PlayCard _ _ _ payment _ False | Cost.wasDreamed payment -> pure g`. The play is complete
  once the dreams are placed.
- The flip dispatches `PutCardIntoPlay`, which branches on card type and pushes
  `InvestigatorPlayEvent` / `InvestigatorPlayAsset` directly. It never calls
  `getIsPlayable`, so no fast window, criterion or limit is consulted.

## Consequence for card implementations

A dreaming card's `cdFastWindow` and `cdCriteria` describe only the facedown play. Anything
the card must check **at resolution** has to be checked by the card's own `RunMessage`
handler. 14026's "Limit 1 per location" is checked there, when the attach target is chosen.
