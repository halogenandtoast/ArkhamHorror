# Phase 6 (conditional): backend single-option trims

Only if phases 1-3 show a stop the frontend cannot handle cleanly (e.g. one round-trip per investigator for the encounter draw makes the flow visibly stutter).

## Candidate
`ForInvestigator iid AllDrawEncounterCard` (`backend/arkham-api/library/Arkham/Game/Runner.hs` ~3470): when `drawTargets == [EncounterDeckTarget]`, push `drawEncounterCard iid' GameSource` directly instead of a one-option `chooseOne`.

## Before changing
- Grep `backend/arkham-api/tests` for specs that answer this `chooseOne`; update them.
- This repo tracks upstream `halogenandtoast/ArkhamHorror`: every backend edit is merge debt. Prefer upstreaming.

## Done when
Backend tests green; the frontend policy rule for encounter draw stays as fallback.
