---
title: Forest of Illusion's free ability connects Circus Encampment to the unveiled location
date_added: 2026-08-25
source: internal decision (user instruction) — campaign-specific ruling for the Circus Ex Mortis homebrew campaign
affects:
  - Forest of Illusion
  - Circus Encampment
  - The Primrose Path
---

# Forest of Illusion's free ability connects Circus Encampment to the unveiled location

**Q: When an investigator activates Forest of Illusion's `[free]` ability at a
Moonlit Forest location, what happens to that location's connections?**

A: In addition to blanking the location's printed text, activating the
ability additionally connects Circus Encampment to that location, both ways.
This is the intended mechanism by which Circus Encampment reaches its
printed 5-connections requirement: at setup, Circus Encampment starts with
only 3 connections, from the three top-row Moonlit Forest copies placed
during The Primrose Path's setup (see `ThePrimrosePath.hs`'s `topRow`
setup). Each subsequent activation of the act's free ability opens one more
route toward the camp, gradually building out Circus Encampment's
connections as the illusions are unworked.

## Affected cards / systems

- Forest of Illusion (`:circus-ex-mortis:020`) — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Acts/ForestOfIllusion.hs`
- Circus Encampment (`:circus-ex-mortis:024`) — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Locations/CircusEncampment.hs`
- The Primrose Path setup (`topRow` connections) — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Scenarios/ThePrimrosePath.hs`

## Implementation status

- **Forest of Illusion**: ✅ correct as written — `UseThisAbility iid (isSource attrs -> True) 1` already calls `connectBothWays camp loc` after blanking the location and placing the horror reminder token.
- This entry ratifies the existing `connectBothWays camp loc` call as the intended mechanism, so it is no longer an uncited deviation.
