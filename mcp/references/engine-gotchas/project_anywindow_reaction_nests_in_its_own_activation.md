---
title: project_anywindow_reaction_nests_in_its_own_activation
description: A freeReaction AnyWindow ability interrupts every window and is re-offered inside its own ActivateAbility window; nonBlocking is the fix, not a use limit
---

A `{reaction}` whose printed text states only a condition -- "If you are in a Sarcee
location: ...", "If there are no more clues on this location: ..." -- is implemented as
`freeReaction AnyWindow` with the condition as `restricted`/`controlled` criteria, not as a
`triggered` ability on an invented window. The condition is a state, not a moment, so any
window where it holds must offer it. Against the Wendigo's Temple of Ithaqua and Lost Child
both read this way.

Left bare, such an ability stops *every* window: each placed token, each would-be-discarded,
each leave-play. Worse, activating it opens `CheckWindows [When, ActivateAbility ...]`, which
is a *fresh* window that `AnyWindow` also matches, so it is offered again on top of itself
before its own `UseThisAbility` resolves. The default `PlayerLimit PerWindow 1` does not help
-- the nested window is a different window. A `--trace` of a replay that keeps answering
"use it" shows `PayForAbility` / `PaidAllCosts` cycling with no sign of the handler's own
messages.

The fix is `nonBlocking` (`Arkham.Ability`), which is built for this: `questionIsOnlyNonBlocking`
in `Game/Runner.hs` drops any ask made up solely of non-blocking reactions plus the Skip
Triggers button. The incidental windows go quiet and the nested activation ask disappears,
while the ability still rides along in a real player window, whose ask also carries component
labels and the end-turn button and so is never "only non-blocking". `nonBlockingExcept` keeps
it blocking in named windows (Safeguard (2) does this for `TurnBegins`).

A use limit such as `onlyOnce` also happens to suppress the nesting, but it invents a
restriction the card does not print and leaves the window noise untouched. Reach for it only
when the printed effect genuinely is once per game.

Related: [[used-ability-record-keeps-original-limit]], [[project_spentclues_window_before_deduction]].
