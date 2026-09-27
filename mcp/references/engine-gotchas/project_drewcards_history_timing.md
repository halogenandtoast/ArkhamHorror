---
title: project_drewcards_history_timing
description: On-draw forced abilities see the just-drawn cards already in PhaseHistory/RoundHistory
---

The `DrewCardsFromOwnDeck` window is fired from the `UpdateHistory iid (HistoryItem HistoryCardsDrawn n)` handler (`Game/Runner.hs` ~3500), but that same handler commits the phase-history increment (`phaseHistoryL %~ insertHistory`) before the window frames are processed. The window only fires on the *first* draw of each phase (`currentCount == 0`).

Consequence: by the time a forced on-draw ability resolves, the current draw is **already counted** in PhaseHistory, and therefore in `RoundHistory` (= roundHistory <> phaseHistory, see `GameEnv.hs` getHistory). So `getHistoryField RoundHistory iid HistoryCardsDrawn` is never 0 at that point — it's already `n`.

To detect "first draw of the round" inside such an ability, compare `RoundHistory` against `PhaseHistory`: equal means no earlier phase drew cards this round. roundHistory accumulates completed phases (`roundHistoryL %~ (<> phaseHistory)` at phase end), phaseHistory is current-phase only.

This was the bug behind Psychotropic Spores (10740) not dealing direct horror (issue #4808): it guarded on `RoundHistory == 0`, which was always false.
