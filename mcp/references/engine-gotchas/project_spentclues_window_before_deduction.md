---
title: project_spentclues_window_before_deduction
description: "InvestigatorSpendClues used to fire its SpentClues After-window before deducting clues, letting forced AnyWindow objectives re-trigger mid-payment"
---

`InvestigatorSpendClues` (Arkham/Investigator/Runner.hs ~1843) used to `push (Do/DoStep spend)` then `pushM (checkAfter SpentClues)` — because push prepends, the "After SpentClues" window ran BEFORE the clue was actually deducted (the `Do` does the `subtractTokens`). So window reactions saw stale (pre-spend) clue counts.

This broke `A Familiar Pattern` (act 53046, Return TFA / Boundary Beyond): its `Objective $ ForcedAbilityWithCost AnyWindow (GroupClueCost (PerPlayer 2) Anywhere)` paid as a multi-investigator split (e.g. 3+1=4). Paying the first investigator's clue opened the SpentClues window while clues still looked affordable, so the forced objective re-triggered mid-payment, double-spent, and left an orphaned sub-payment that threw `InvalidState "Can't afford cost (b): Costs [ClueCost (Static 3)]"` (issue #4937). Single-investigator costs never hit it (after one spend nobody can afford the re-trigger).

Fix: deduct first, window after — `afterWindow <- checkAfter ...; pushAll [Do/DoStep, afterWindow]` (engine convention; cf. Runner.hs ~2008 `pushAll [..., afterWindow]`). Broad: InvestigatorSpendClues is used ~24 places, so the corrected post-deduction timing must pass the full suite.

The GroupClueCost split itself lives in ActiveCost.hs ~1189 (`sum == totalClues` branch pushes per-investigator `PayCost (ClueCost (Static cCount))`). Related: [[project_simultaneous_damage_window_targets]].
