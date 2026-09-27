---
title: "\"Fool me once...\" discards another investigator's signature weakness when it attaches"
date_added: 2026-06-27
source: designer ruling (Alex, Chapter 2 rules question — Signature Cards rule)
affects:
  - "\"Fool me once...\" (06156)"
  - The Harbinger (08006)
  - Prophecy of the End (11016)
  - Signature Cards rule
  - signature weakness
---

# "Fool me once..." discards another investigator's signature weakness when it attaches

The Signature Cards rule in FAQ 2.5 states: "If a game effect would force a
player to take control of a card with another investigator's signature card
attached to it, that signature card is discarded." Chapter 2's rulebook adds:
"If a game effect...attached to it, that effect fails and the signature card is
discarded instead."

Q: Does this rule affect "Fool me once..." (`06156`)?
1. If Roland triggers an action ability on Norman's **The Harbinger** (`08006`)
   and would discard it, can he play "Fool me once..." to attach it?
2. If Roland draws/resolves Gloria's **Prophecy of the End** (`11016`), can he
   play "Fool me once..." to attach it?

A (designer): The controller of "Fool me once..." cannot have another
investigator's signature weakness attached to the card. Roland playing "Fool me
once..." after triggering The Harbinger or resolving Prophecy of the End will
result in **"Fool me once..." entering play, the signature weakness attaching,
and then the signature weakness immediately discarding.**

So the play is legal, the event still enters play, but the foreign signature
weakness is discarded (to its owner) instead of remaining attached. "Fool me
once..." ends up in play with nothing attached. (Attaching the controller's
**own** signature weakness is unaffected.)

## Affected cards / systems

- "Fool me once..." (06156) — `backend/arkham-api/library/Arkham/Event/Events/FoolMeOnce1.hs`
- The Harbinger (08006), Prophecy of the End (11016) — example foreign signature
  weaknesses that can trigger this; no card-specific changes needed.

## Implementation status

- **"Fool me once..." (06156)**: ✏️ updated. `PlayThisEvent` now checks the
  treachery being taken: if it is owned by another investigator **and** is a
  signature card — `isSignature` (signature assets/events) or
  `cdCardSubType == Just Weakness` (story/signature weaknesses like The Harbinger
  and Prophecy of the End, which do *not* carry a `Signature` deck restriction) —
  it is sent to its owner's discard via `addToDiscard` instead of
  `PlaceUnderneath`. The event still enters play. The controller's own signature
  weakness still attaches normally. (`FoolMeOnce1.hs`)

  N.B. The first pass used only `isSignature`, which is **false** for signature
  weaknesses (they use the `Weakness` subtype, not the `Signature` deck
  restriction) — so the `cdCardSubType == Just Weakness` branch is the one that
  actually fires for the FAQ's two examples.

- **Verified** in the running app: a Roland solo game was driven to the
  `TreacheryWouldBeDiscarded` window for Prophecy of the End (`11016`) with
  "Fool me once..." in hand, exported, and re-imported. Playing the event
  leaves it in play with **nothing** attached and Prophecy in the discard
  (pre-fix it stayed attached underneath). Debug file:
  `~/Downloads/fool-me-once-foreign-signature.json`.
