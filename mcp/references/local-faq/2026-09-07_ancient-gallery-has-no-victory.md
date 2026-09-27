---
title: Ancient Gallery has no Victory value
date_added: 2026-09-07
source: Issue #5629 — reporter confirmed against the physical card
affects:
  - Ancient Gallery
---

# Ancient Gallery has no Victory value

**Q: Does Ancient Gallery (11548, The Drowned Quarter) award a victory point?**

A: No. The printed card has no Victory value. ArkhamDB / arkham.build report `victory: 1` for
11548, but the card image — and the physical card — show none. Treat the upstream data as wrong.

Do not re-add `victory 1` to this card def when reconciling against `.claude/data/cards.json`.

The sibling Coral Reef locations (11546, 11547) legitimately are Victory 1 and are unaffected.

## Affected cards / systems

- Ancient Gallery (11548) — `backend/arkham-api/library/Arkham/Location/CardDefs/TheDrownedCity/TheDrownedQuarter.hs`

## Implementation status

- **Ancient Gallery (11548)**: ✏️ updated. Removed `victory 1` from the card def; the location no
  longer enters the victory display or contributes XP at scenario end.
