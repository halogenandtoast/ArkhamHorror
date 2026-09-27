---
name: project_haunted_ask_baked_before_cancel
description: "ST.6 baked the Haunted chooseOneAtATime into the same pushAll as the When FailedSkillTest messages, so Neither Rain nor Snow's CancelEffects arrived too late; now deferred behind ResolveHauntedAbilities (#5516)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 277922f0-eeba-412d-9072-5b6d9da088dc
  modified: 2026-08-26T06:20:59.657Z
---

`SkillTestApplyResults` (ST.6, `Arkham/SkillTest/Runner.hs`) used to `select` the location's
Haunted abilities up front and bake the `chooseOneAtATime` into the *same* `pushAll` as the
`When (FailedSkillTest …)` messages. Neither Rain nor Snow reacts to that `When` and applies
`CancelEffects` to the skill test — by which point the Haunted ask was already queued, and
nothing re-checked the modifier. Haunted resolved through the cancel (#5516).

Fixed by pushing `ResolveHauntedAbilities iid lid` (a new `SkillTestMessage` variant +
pattern synonym in `Arkham.Message`) in that slot; its handler re-selects the abilities and
applies the same gate ST.7 uses:
`CancelEffects `elem` modifiers && EffectsCannotBeCanceled `notElem` targetMods`.

**Why:** Alert and Retaliate were never affected — they hang off the *unwrapped*
`FailedSkillTest … (Initiator target)` pushed in `SkillTestApplyResultsAfter`
(`Enemy/Runner.hs`), and that whole block already sits inside `unless cancelled`. Haunted was
the only ST.6-resident failure effect, so it was the only one outside the gate. FAQ Q65 says
Neither Rain nor Snow cancels effects in **Steps 6 & 7**, alert/retaliate/haunted included.

**How to apply:** anything that resolves as a consequence of a *failed* skill test and is
pushed during ST.6 must be deferred behind its own message if a card could cancel it in the
`When` windows. Same class as [[project_skilltest_option_messages_baked_early]] and
[[project_if_window_payload_must_outlive_the_effect]].

Still unfixed: the FAQ's "negative effects on the scenario reference card" half of Q65 —
those are per-scenario and don't route through the skill-test runner.

Verifying with `arkham-replay`: `--undo N` alone never re-runs ST.6 (it restores to a stop
point and drains). For #5516 the path was `--undo 2` → answer the failure window → answer
`SkillTestApplyResultsButton`; only then does ST.6 execute under the new code.
See [[project_arkham_replay_tool]].
