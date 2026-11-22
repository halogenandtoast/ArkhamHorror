---
title: Facedown hiding is client-side only — do not build per-viewer redaction
date_added: 2026-11-21
source: internal decision (project owner)
affects:
  - dreaming
  - Power of the Dreamlands
  - concealed
  - hidden cards
---

# Facedown hiding is client-side only

A facedown card's identity is hidden **in the client only**, and that is the house
standard. Decks, hands, and the whole `gameCards` registry are already published to every
client, and there is no per-viewer encoding — the room gets **one payload**. Do not build
per-viewer redaction for the dreaming keyword (or for any other facedown mechanic) on that
feature's budget.

Settled so it is not re-litigated per card.

## Affected cards / systems

- the per-room payload — `publishToRoom`; one encoding per room, not per seat
- the investigator encoder — `backend/arkham-api/library/Arkham/Game.hs` (`PublicGame`
  per-investigator `additionalData`); note it has **two** encoders that have already
  drifted, so a new top-level key is not the place for anything
- the dream zone wire shape — `dreamingCards` on the investigator;
  `frontend/src/arkham/types/DreamingCard.ts`
- the log — `backend/arkham-api/library/Arkham/Log/Narrator.hs`

## Implementation status

- **engine**: ✅ no redaction. `DreamingCard` encodes its whole `Card`, the same way a hand
  card does.
- **log**: ✅ the one narration the dream zone adds (`log.cannotDreamAnotherCard`) does not
  name the card. The log is shared, so a line naming a card about to sit facedown would give
  away something the client is trying to hide — a cheap courtesy, not a security boundary.
- **frontend**: ✅ hiding and the hover-to-peek affordance live in
  `frontend/src/arkham/components/DreamingCard.vue`. Treat it as a convenience, not
  secrecy.
- **do not**: ⚠️ add a per-seat payload, a `Field` that strips card identity, or a
  "visible to owner" flag on `DreamingCard`. If genuine hidden information is ever wanted,
  it is its own project and needs a per-viewer encoding path first.
