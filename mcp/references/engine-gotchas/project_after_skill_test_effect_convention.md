---
name: project_after_skill_test_effect_convention
description: "How to handle an after-skill-test effect: the SkillTestEnded window is wholly post-ST.8, so it outlives its test and keeps answering until the seat declines"
metadata:
  node_type: memory
  type: project
---

`Window.SkillTestEnded` fires **wholly after ST.8** — outside the test's own steps. That one fact
settles how everything declared there behaves, and it is the convention to build to from now on.

## The rules reading

Because the trigger is outside the test, FAQ 1.17 (*a skill test cannot initiate during another
skill test*) does **not** apply to anything declared in this window. A test initiated there is an
ordinary nested sequence under FAQ 1.4: it resolves completely, and then the window **continues**,
still answering the same triggering condition. So a second copy of a card can answer the same
failure again, and the innermost test's own window is offered before the outer one's (LIFO).

Do not reach for 1.17 to justify deferring or closing this window. 1.17 governs a test initiated
from *within* ST.1–ST.8 (Expose Weakness during an investigate); this window is past that.

## What that costs the engine

Two facts make the window outlive what it reports on:

- `Msg.SkillTestEnded` clears `gameSkillTest` (`Game/Runner.hs`), and the runner pushes it
  *before* `EndSkillTestWindow` — the sentinel a deferred test is anchored behind. By the time a
  nested test finishes and the window resumes, the test it reports on is gone, so any message
  routed by `SkillTestId` to the live entity (`RepeatSkillTest`) matches nothing.
- The window's `Do (CheckWindows ws)` re-check (`Question.hs`, the `WindowAsk` handler) is the
  **only** copy of the ask, and it rebuilds every seat from scratch. Deleting it closes the window
  for all seats and every other card in it — not just the one you were thinking about.

## The convention

1. **Never delete the re-check to stop a card re-triggering.** Per-ability use limits already do
   that. If a card must not answer twice, say so on the ability, not on the queue.
2. **Carry the ask, don't re-emit it.** `popMessagesMatching` it and re-insert it behind whatever
   you queued; a freshly built window is not the same window.
3. **Re-seat the test around the carried ask** with
   `RestoreSkillTestForWindow (Just st)` / `RestoreSkillTestForWindow Nothing`, contiguous with it.
   Cards in the window read the live test for costs and criteria, and it has torn down by then.
4. **Read the window's own snapshot where you can.** `Matcher.SkillTestEnded` matches against the
   `SkillTest` the window carries, and `SkillTestWasFailed` reads `skillTestResult st`, so
   playability needs no re-seating. Prefer that over `getSkillTest` for anything in this window.
5. **Place a nested test after the declaring card's tail.** Anchor on `EndSkillTestWindow` while it
   is still queued; once the test has been re-seated there is no sentinel left, so use
   `replaceMessageMatching` to drop it where the ask sat. Pushing it to the front instead makes the
   declaring card discard *after* the nested test — the #5744 symptom.
6. **Riders teardown at `SkillTestEnded`, never `SkillTestEnds`** — see
   [[project_onsucceedby_rider_repeat_skilltest]] for the rider half of this, including the
   `st.source == a.source` gate that decides what a repeat carries over.

## Testing it

A seat that answers with `SkipTriggersButton` keeps `skippedWindow` set, so a test that follows
auto-advances past its own commit step: a spec must not expect a `StartSkillTestButton` after a
`skipAcrossQuestions`. `arkham-replay --undo 1 --answers` reproduces the whole chain from a `/file-bug`
export; `gameSkillTest` in the output is how you check the re-seating actually happened.

Worked through on Live and Learn for #5744 (which over-corrected by deleting the re-check) and
#5822 (the same user asking for the window back).

Related: [[project_onsucceedby_rider_repeat_skilltest]], [[project_open_windows_live_in_two_places]],
[[project_st7_option_criteria_reevaluated_per_round]]
