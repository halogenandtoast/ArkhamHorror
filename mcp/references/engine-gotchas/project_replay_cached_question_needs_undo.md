---
name: project_replay_cached_question_needs_undo
description: "arkham-replay without --undo re-emits the export's saved gameQuestion verbatim, so ability-availability fixes look like no-ops"
metadata: 
  node_type: memory
  type: project
  originSessionId: 7c50f7fe-3f2d-4d61-9d80-7b0f8c770376
  modified: 2026-07-29T07:45:33.715Z
---

`arkham-replay <export> --output state.json` drains the saved queue. If the queue is
already empty (the usual case for a `/file-bug` export, captured while a question is
pending), the engine has nothing to run and **re-emits `gameQuestion` exactly as
serialized**. Ability lists are not recomputed.

Consequence: a fix to what appears in a player window (ability availability, action
legality, playable cards) produces a byte-identical choice list before and after the
change — even when the recompiled binary demonstrably contains the fix. Verified on
#5282: with the ability's criteria override forced to `Never`, the offending Fight
action still appeared in the no-undo run; `--undo 1` immediately dropped it.

**How to apply:** to verify anything question-shaped, always `--undo N` (N≥1) so the
question is rebuilt from the queue. Confirm the fix is actually in the binary by
reading the relevant entry out of `.gameModifiers` in the output JSON rather than
trusting mtimes — the modifier map is serialized with its live criteria.

**`--undo N` alone is often still not enough (#5398).** Rewinding to step K restores that
step's *stored* `gameQuestion` from the patch; if the queue at K is already drained, the
engine re-emits it without recomputing the ability list — same failure mode as the no-undo
case, just one step earlier. On #5398 `--undo 4` landed exactly on the Safeguard reaction
window and pre-fix/post-fix choice lists were identical, while `.gameModifiers` in the very
same output proved the new `CannotEnter` was live on the right investigator.

The reliable shape is **undo past the question's creation, then replay forward**: pick N so
the pending question is the one *before* the window you care about, answer it via
`--answers`, and let the engine rebuild the next question from the queue. `--undo 5 +
--answers` immediately showed the Safeguard window present pre-fix and gone post-fix.
Rule of thumb: if pre/post outputs are byte-identical but `.gameModifiers` shows the fix,
you are one `--undo` too shallow — increase N by one and drive forward.

Corollary for positive tests: `--undo` also rewinds the actions/cards the user spent,
so the entity you want to exercise may not be in play yet at depth N. Inject
`{"tag":"Raw","contents":{"tag":"GainActions","contents":["<iid>",{"tag":"GameSource"},2]}}`
as the first answer to buy actions without rewinding further. Every `Raw` bumps
`gameScenarioSteps`, so each subsequent `Answer` needs the version from the *previous*
run's output — build the answers file incrementally.

Related: [[project_stale_local_bin_arkham_replay]],
[[project_replay_undo_entity_token_fidelity]], [[project_action_diff_snapshot]].
