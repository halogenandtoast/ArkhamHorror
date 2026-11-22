---
title: A facedown dreaming card is not in play and is not an entity
date_added: 2026-11-21
source: internal decision (project owner)
affects:
  - dreaming
  - Power of the Dreamlands
---

# A facedown dreaming card is not in play and is not an entity

A card played facedown with the **dreaming** keyword is **not in play**, and it is **not
an entity**. It is held as a lightweight record in a new investigator-side dream zone, not
as an Asset or an Event. No `HasAbilities` and no `HasModifiersFor` ever runs on a facedown
dreaming card.

## Affected cards / systems

- `dreaming` keyword — `backend/arkham-api/library/Arkham/Keyword.hs` (`Dreaming Int`)
- the dream zone — `investigatorDreamingCards :: [DreamingCard]` on `InvestigatorAttrs`
  (`backend/arkham-api/library/Arkham/Investigator/Types.hs`), record type in
  `backend/arkham-api/library/Arkham/DreamingCard.hs`
- addressing — `DreamingCardTarget CardId` / `DreamingCardSource CardId`
  (`Arkham/Target.hs`, `Arkham/Source.hs`)
- matchers — `DreamingCardMatcher` (`Arkham/Matcher/DreamingCard.hs`)

## Implementation status

- **dream zone**: ✅ implemented as a record list, not an entity map. Nothing adds it to
  `Entities`, so no ability/modifier collection pass can reach it.
- **addressing**: ✅ deliberately NOT `CardIdTarget`. `CardIdTarget` is "the one target that
  matches the card in every zone", and reusing it would make the dream zone addressable as
  if the card were in play — the #5695 bug class (`AssetControlledBy` matching hand cards).
- **matchers**: ✅ `DreamingCardMatcher` has exactly two predicates,
  `DreamingCardControlledBy` and `DreamingCardWithDreams`. There is deliberately **no**
  `DreamingCardWithCard`: no card in the Power of the Dreamlands set reads a facedown
  dreaming card's identity (every identity read happens while the card is in hand or in
  deck), and omitting it is the structural guarantee for this ruling. Do not add it without
  a printed card that needs it.
