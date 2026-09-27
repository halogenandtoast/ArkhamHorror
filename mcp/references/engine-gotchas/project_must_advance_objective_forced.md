---
title: project_must_advance_objective_forced
description: "\"you must advance at end of round\" objectives need `forced`, not `freeReaction`, or the SkipTriggersButton lets players skip the mandatory advance"
---

Act/agenda objectives whose card text says "you **must** advance" must be coded as
`Objective $ forced $ RoundEnds #when`, NOT `Objective $ freeReaction $ RoundEnds #when`.

**Why:** a `freeReaction` at a window is presented as `ChooseOne [AbilityLabel, SkipTriggersButton]`,
so the player (or "skip triggers for all") can skip the mandatory advance. `forced` auto-resolves
with no skip button. The advance handler (`UseThisAbility ... -> advancedWithOther attrs`) is
identical for both, so the fix is a one-word swap.

**How to apply:** grep the act/agenda for `freeReaction $ RoundEnds`; if the printed text says
"must advance", change to `forced`. Correct exemplars: `FaceTheMusic.hs`, `BlackwatersBane.hs`.

Fixed issue #5076: The Blob That Ate Everything act 2 Extraterrestrial Physiology (`85007`) —
Vulnerable Heart survived because the round-end advance was skippable. See
[[project_drewcards_history_timing]] for round/phase-window nuance and
[[project_skip_all_triggers]] for SkipTriggersButton orchestration.
