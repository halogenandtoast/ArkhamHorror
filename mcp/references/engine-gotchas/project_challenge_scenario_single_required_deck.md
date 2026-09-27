---
title: project_challenge_scenario_single_required_deck
description: Challenge side stories need only ONE player on the required deck; deck validation enforces the required investigator only on the last player choosing
---

Challenge side stories (`challengeScenarioInvestigators` in `frontend/src/arkham/deckRestrictions.ts`, e.g. Read or Die 90004 → Daisy Walker) require the named investigator, but in multiplayer only ONE player must use that required deck — not every player.

`deckRestrictionError(scenarioId, deckList, chosenInvestigatorCodes, campaign, t, { isLastPlayer })`:
- If the deck IS the required investigator → enforce its `scenarioDeckRestrictions` (e.g. Daisy's 4+ non-weakness Tomes).
- If the deck is NOT the required investigator → only error when `isLastPlayer` AND no `chosenInvestigatorCodes` already satisfy it (nobody else can provide it).
- `isLastPlayer` defaults to `true` so single-player stays strict.

Callers: `ChooseDeck.vue` computes `isLastPlayerChoosing` = (EmptyPlayer count ≤ 1) and passes already-chosen investigators. `UpgradeDeck.vue` passes the rest of the group + `isLastPlayer: false` (the required investigator was already validated at scenario start, so upgrades never re-block on identity).

The campaign add-side-story menu (`ContinueCampaign.vue` `standalones`) already uses `find` on `requiredInvestigator` (one player) — no change needed there. Backend (`spendSideStoryXp`, `LoadScenario`) already uses `selectAny`/non-empty `select InvestigatorWithTitle` = one player; the required investigator pays full XP, others pay 1.
