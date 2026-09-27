---
title: A Moonlit Forest search-cost move is legal even when the search can find nothing
date_added: 2026-08-25
source: Internal decision (user ruling, 2026-08-25)
affects: [Moonlit Forest, Smoldering Campfire, Quiet Valley, additional cost, search the encounter deck]
---

# A Moonlit Forest search-cost move is legal even when the search can find nothing

Smoldering Campfire and Quiet Valley charge "search the encounter deck and discard pile for
&lt;X&gt;" as an additional cost to move to another [[Woods]] location. If neither the encounter deck
nor the discard pile contains a matching card, the investigator **may still move** — the search is
performed, finds nothing, and the cost is considered paid.

The stricter reading (no match ⇒ the cost cannot be paid ⇒ the move is illegal) is **rejected**: it
can strand an investigator in the Moonlit Forest for reasons unrelated to the card's intent.

## Affected cards / systems

- Moonlit Forest — Smoldering Campfire (`:circus-ex-mortis:025`) — file:
  `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Locations/MoonlitForestSmolderingCampfire.hs`
- Moonlit Forest — Quiet Valley (`:circus-ex-mortis:026`) — file:
  `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Locations/MoonlitForestQuietValley.hs`
- Cost: `FindEncounterCardCost` — files: `backend/arkham-api/library/Arkham/Cost.hs`,
  `backend/arkham-api/library/Arkham/Helpers/Cost.hs`

## Implementation status

verified — `getCanAffordCost_` answers `FindEncounterCardCost` with `can.target.encounterDeck iid`
only; it does not require a matching card to exist. This mirrors the affordability of
`DiscardEncounterUntilFirstCost`, the engine's other search-shaped cost.
