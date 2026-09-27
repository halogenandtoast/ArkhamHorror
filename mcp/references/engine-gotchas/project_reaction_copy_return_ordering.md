---
title: project_reaction_copy_return_ordering
description: "A single-use reaction that copies an ability then returns itself (Spiritual Echo 2) must returnToHand BEFORE performing the copy, or it self-recurses"
---

Spiritual Echo (2) (`SpiritualEcho2.hs`, event `11075`, The Drowned City) attaches to your
location. Reaction: "After you activate an [action] or [free] ability on a Spell or Ritual
asset: Perform that ability again at attached location ... ignoring that asset's [action]
cost ... Return Spiritual Echo to your hand." It is SINGLE USE — echoes one ability, then
returns to hand (immediately, per printed text; there is no "end of round" clause despite
a first impression — confirmed with the user against the physical card).

Original handler pushed the copied `UseAbility` first and `returnToHand` second. Because
the copied `UseAbility` opens a fresh `After ActivateAbility` window while the event is
STILL in play, the reaction re-triggers on its own echo; each trigger queues another
`[UseAbility, ReturnToHand]` for the SAME event. On unwind the first `ReturnToHand`
removes the entity and every later one crashes `getEvent` → `Unknown event …`
(`Runner.hs` `ReturnToHand (EventTarget …)`). Issue #4941 (Borrowed Time + Time Warp +
Sefina, nested 7 deep).

**Fix (order-preserving):** keep the printed order — perform the copy, THEN
`returnToHand iid attrs` — but for the duration of the copied `UseAbility` add
`CannotTriggerAbilityMatching (AbilityIs (toSource attrs) 1)` to the activating
investigator alongside the existing `AsIfAt lid` (use `temporaryModifiers` with both
mods). `temporaryModifiers` expands to `[CreateEffect, <body>, DisableEffect]`, so the
guard is live throughout the copy's "after you activate" window and disabled before
`ReturnToHand`. The event's own ability is therefore not re-offered on its own echo →
exactly one echo, one return, no crash, with the return still happening last. (A simpler
"return BEFORE the copy" reorder also fixes the crash, but the user wants the printed
order preserved.) General rule for any single-use "copy an ability then bounce this
card" reaction.

**Verification gotcha:** the bug export captured the recursion ALREADY baked into the
committed queue, so replaying it raw — even `--undo` into the middle, or `--replay-all` —
still crashes on the old pending `ReturnToHand`s regardless of the fix. Must `--undo` to
BEFORE the FIRST reaction trigger (the echo window with `ActivateAbility` nesting depth 1;
here ver38), re-take it under new code, and confirm exactly one `ReturnToHand` + no crash.
Relates to [[project_cancelenemydefeat_queue_layer]].
