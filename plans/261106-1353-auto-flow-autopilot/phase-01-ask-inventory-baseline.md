# Phase 1: Ask inventory + click baseline

## Why
Grepping only guesses. The policy must be built from the stops that actually appear. `arkham-replay --replay-all` does not re-apply answers, so the inventory is captured live.

## Steps
1. Dev-only ask logger in `frontend/src/arkham/views/Game.vue` (behind `import.meta.env.DEV` or the debug flag): on every new question record `{round, phase, phaseStep, playerId, questionTag, choiceTags[], validChoiceCount, labels}` and the index the player picked. Export as JSON from the debug menu.
2. Play one full solo round of The Gathering (Roland, no fast cards held) plus the scenario opening (deck pick, mulligan, setup), then one round of a 2-investigator multiplayer-solitaire game.
3. Classify each stop: trivial (no decision) / real decision / narrative.
4. Record baseline clicks per phase in `reports/ask-inventory-baseline.md` inside this plan folder.

## Done when
Inventory table exists, every row classified, baseline numbers recorded. Logger stays behind the debug flag only.

## Risk
Other investigators/scenarios add stops not seen here. The policy is an allow-list, so unseen shapes simply stop as today.
