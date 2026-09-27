---
title: Transfiguration cannot choose bonded investigator cards
date_added: 2026-06-06
source: internal decision (maintainer ruling, GitHub issue #4724)
affects:
  - Transfiguration (11076)
  - Hank Samson, Resolute (10016a/10016b)
  - The Lost Homunculus (11068b)
  - bonded
---

# Transfiguration cannot choose bonded investigator cards

Transfiguration (2) (`11076`) — "Choose another investigator card from your
collection" — cannot choose bonded investigator cards, e.g. Hank Samson,
Resolute (`10016a`/`10016b`) or The Lost Homunculus (`11068b`).

Rationale:

1. Bonded cards only enter the game via the card they are bonded to (bonded
   keyword); they are not deckbuilding/collection choices.
2. A double-faced bonded investigator card (Resolute Hank has gameplay faces
   on both sides) has no single defined "front" for Transfiguration's "treat
   the front of your investigator card as if it were the front of the chosen
   card".

Transfiguring into regular Hank Samson (`10015`) **is** legal, including his
defeat reaction: if a transfigured investigator would be defeated, the
reaction heals all damage/horror and the treated-as front becomes the chosen
side of the Resolute card.

## Affected cards / systems

- Transfiguration (11076) — `backend/arkham-api/library/Arkham/Event/Events/Transfiguration2.hs`
- Hank Samson (10015/10016a/10016b) — `backend/arkham-api/library/Arkham/Investigator/Cards/HankSamson.hs`
- Investigator registry — `backend/arkham-api/library/Arkham/Investigator.hs`, `backend/arkham-api/library/Arkham/Investigator/Cards.hs`

## Implementation status

- **Transfiguration (11076)**: ✅ matches ruling — `Transfiguration2.hs` filters bonded codes `["10016a", "10016b", "11068b"]` out of the choice list.
- **Hank Samson (10015)**: ✅ choosable while transfigured — `hankSide` helper dispatches on `TransfiguredForm`; defeat reaction swaps the form to `TransfiguredForm "10016a"/"10016b"`, backed by registered Resolute investigator builders with correct stats.
- Verified end-to-end via `arkham-replay` against the issue #4724 export (bonded cards absent from picker; 10015 transfiguration and defeat-swap run without crashing).
- Regression test: `backend/arkham-api/tests/Arkham/Event/Events/Transfiguration2Spec.hs` (bonded cards not offered; transfiguring into Hank 10015 applies his stats).
