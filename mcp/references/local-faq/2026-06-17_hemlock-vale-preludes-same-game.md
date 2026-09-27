---
title: Hemlock Vale Preludes are the same game as the scenario that follows
date_added: 2026-06-17
source: FFG FAQ (The Feast of Hemlock Vale)
affects:
  - The Feast of Hemlock Vale (campaign "10")
  - Prelude scenarios (10704, 10677a, 10679a, 10679b)
  - once per game abilities
  - "when the game begins" windows
---

# Hemlock Vale Preludes are the same game as the scenario that follows

> **Q:** Are the Preludes in The Feast of Hemlock Vale considered to be the same
> game or a different game than the scenarios played after them, for the purpose
> of setup or "once per game" abilities?
>
> **A:** Preludes are considered part of the same game as the scenario that
> follows them. Only resolve setup abilities once per Prelude & scenario pair.

Consequences for the engine:

1. **"Once per game" ability usage must NOT be cleared** when transitioning from a
   Prelude into the scenario that follows it. A `PerGame` ability used during the
   Prelude stays used for the following scenario (and vice versa).
2. **Start-of-game windows must be skipped** in the scenario that follows a Prelude
   — the game already "began" during the Prelude, so `GameBegins` ("when the game
   begins") windows do not fire again, and opening-hand revelations do not
   re-resolve.

## Affected cards / systems

- `afterPrelude` transition: `backend/arkham-api/library/Arkham/Campaigns/TheFeastOfHemlockVale/Helpers.hs`
- Scenario options: `backend/arkham-api/library/Arkham/Scenario/Options.hs`
- Campaign step dispatch (skips investigator reset): `backend/arkham-api/library/Arkham/Campaign/Runner.hs`
- `EndSetup` → `BeginGame` push: `backend/arkham-api/library/Arkham/Scenario/Runner.hs`
- `BeginGame` → `GameBegins` window: `backend/arkham-api/library/Arkham/Game/Runner.hs`
- `ForInvestigators _ ResetGame` (clears non-`PerCampaign` used abilities): `backend/arkham-api/library/Arkham/Investigator/Runner.hs`

## Implementation status

- **Once-per-game preservation**: ✅ already matched ruling — no change. Post-Prelude
  scenarios are loaded via `afterPrelude`, which sets
  `scenarioOptionsSkipInvestigatorSetup = True`. In `Campaign/Runner.hs` the
  `ForInvestigators [] ResetGame` message (the only thing that filters
  `investigatorUsedAbilities` down to `onlyCampaignAbilities`, dropping all `PerGame`
  usage) is gated on `not opts.skipInvestigatorSetup`, so it is not pushed after a
  Prelude. `PerGame` usage therefore carries over.
- **Skip start-of-game windows**: ✏️ updated. Added
  `scenarioOptionsSkipStartOfGame` to `ScenarioOptions` (default `False`), set to
  `True` by `afterPrelude`. The `EndSetup` handler now only pushes `BeginGame` when
  this flag is unset, so the `GameBegins` window frame and the per-investigator
  opening-hand revelation pass are skipped for the scenario that follows a Prelude.
