---
title: project_test_addinvestigator_playerorder
description: Test addInvestigator now appends to gamePlayerOrder so getInvestigators (inTurnOrder) sees added investigators
---

`getInvestigators` = `inTurnOrder (select UneliminatedInvestigator)`, and `inTurnOrder` filters to `gamePlayerOrder`. The test game starts with `gamePlayerOrder = [primaryId]` only. `addInvestigator` (TestImport.hs) originally inserted just the entity, so added investigators were invisible to `getInvestigators`/`getInvestigators`-based matchers (SearchAllInvestigators fold, NearestLocationToMost votes).

Fixed by making `addInvestigator` also append `toId investigator'` to `playerOrderL`.

**How to apply:** Do NOT manually re-append to `gamePlayerOrder` after `addInvestigator` — that double-counts the investigator (duplicate turn-order entry → duplicate votes/queries). Just call `addInvestigator`.
