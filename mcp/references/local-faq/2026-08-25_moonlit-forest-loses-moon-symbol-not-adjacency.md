---
title: Moonlit Forest "loses its moon connection symbol" does not break grid adjacency
date_added: 2026-08-25
source: internal decision (user instruction) — campaign-specific ruling for the Circus Ex Mortis homebrew campaign
affects:
  - Moonlit Forest — Circular Grove
  - Moonlit Forest — Misty Marsh
  - Savage Nature
  - Blood Moon
  - connectsToAdjacent
---

# Moonlit Forest "loses its moon connection symbol" does not break grid adjacency

**Q: When Circular Grove or Misty Marsh "loses its moon connection symbol," does
it also stop being connected/traversable to its neighboring Moonlit Forest
locations on the grid?**

A: No. "This location loses its moon connection symbol" only removes the
location from matching effects that specifically key off the printed moon
symbol — for example the Savage Nature/Blood Moon agenda's ability, which
counts or acts on "adjacent copies of Moonlit Forest connected to each other"
via that symbol. It is not a loss of grid adjacency or traversability. The
location remains connected to (and can still be moved to/from) its physical
grid neighbors exactly as before; only the moon-symbol-specific interaction is
affected.

## Affected cards / systems

- Moonlit Forest — Circular Grove (`:circus-ex-mortis:029`) — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Locations/MoonlitForestCircularGrove.hs`
- Moonlit Forest — Misty Marsh (`:circus-ex-mortis:030`) — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Locations/MoonlitForestMistyMarsh.hs`
- Savage Nature / Blood Moon agenda — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Agendas/SavageNature.hs`, `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Agendas/BloodMoon.hs`
- Grid connectivity helper — `connectsToAdjacent`

## Implementation status

- **Moonlit Forest — Circular Grove**: ✅ correct as written — keeps `connectsToAdjacent` for grid traversal; only the moon-symbol-keyed effects are gated separately.
- **Moonlit Forest — Misty Marsh**: ✅ correct as written — same as above.
- This entry ratifies the existing code as intentional, so it is no longer an uncited deviation from a literal reading of "loses its moon connection symbol."
