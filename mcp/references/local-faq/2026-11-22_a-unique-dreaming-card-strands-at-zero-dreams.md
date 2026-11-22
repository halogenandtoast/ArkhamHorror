---
title: A unique dreaming card whose copy is in play strands at 0 dreams
date_added: 2026-11-22
source: internal decision (project owner ruling)
affects:
  - dreaming
  - uniqueness
  - Luke Robinson
  - Dalkura
  - Idol of Bokrug
  - Sigil of Thalarion
  - Staff of Atal
---

# A unique dreaming card whose copy is in play strands at 0 dreams

A facedown dreaming card is not in play, so dreaming a **unique** card is legal even while
another copy of it is in play. Uniqueness bars a card from *entering* play, and the flip
faceup is an enters-play.

When the last dream comes off and a copy is already in play, the flip is **refused**. The
record stays in the dream zone at **0 dreams**, and the refusal is **narrated**. It flips
the moment the other copy leaves play.

This supersedes nothing; it settles the question left open as A2.4 of the dreaming seam
design. It is live for **Luke Robinson (14045)**, which is unique and has dreaming 3.

## Affected cards / systems

- Luke Robinson (14045) — `backend/arkham-api/library/Arkham/Asset/Assets/LukeRobinson.hs`
- the refusal — `FlipDreamingCardFaceup` in
  `backend/arkham-api/library/Arkham/Investigator/Runner.hs`
- the idempotent re-check — `Do (CheckDreamingCard ..)`, same file
- the log line — `backend/arkham-api/library/Arkham/Log/Narrator.hs`
  (`log.cannotFlipDreamingCardFaceup`)

## Implementation status

- **the refusal**: ✅ already matched the ruling — no change. `FlipDreamingCardFaceup`
  checks `cardUniquenessOk` on the registry copy of the card and, when it fails, leaves the
  record in `investigatorDreamingCards` and pushes nothing. The record therefore sits at 0
  dreams rather than being discarded or flipped into an illegal state.
- **flipping later**: ✅ already matched — `Do (CheckDreamingCard ..)` is idempotent (it
  flips iff the record exists and holds no dreams), so when the other copy leaves play any
  pusher of that check flips the stranded card. Nothing extra was added.
- **narration**: ✅ already matched — `Log/Narrator.hs` emits
  `log.cannotFlipDreamingCardFaceup` on exactly this path, off its own key rather than the
  control-limit key, because this is a different refusal.
- **a stranded card at 0 dreams is still a dreaming card you control**: ✅ it counts against
  the limit of 3 and still satisfies "a dreaming card you control" — it is in the zone. Its
  0 dreams mean it cannot be a move source, and it never needs a turn-start removal
  (`RemoveDreamsAtTurnStart` only touches records with dreams on them).
- **teardown**: ✅ the record is cleared on elimination and at end of scenario, so a
  stranded card cannot outlive the card registry and hard-error on the next read.
