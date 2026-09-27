---
title: project_replay_undo_drops_skilltest_modifiers
description: "arkham-replay --undo does not restore skill-test-scoped modifiers, so rewinding into a live skill test silently drops token-value replacements — check skillTestResultsChaosTokensValue before trusting the state"
---

`arkham-replay --undo N` reverses entity state via each step's `choicePatchDown`, but **skill-test-scoped modifiers do not come back**. Rewinding to a point inside a live skill test yields a state where `gameModifiers` has no `ChaosTokenTarget` entries and `gameEntities.effects` is empty, even though the real game had them.

Symptom: the trace's `SkillTestResults_` shows the *unmodified* token value. On #5352, `--undo 10` landed on the "apply results" question with `skillTestResultsChaosTokensValue = -4` (raw [elder_thing]) instead of `-1` (The Black Cat's replacement), so pre-fix and post-fix runs were byte-identical and proved nothing.

**How to apply:** before using an `--undo` state to verify a modifier-related fix, confirm the modifier is actually present — grep the `--trace` for `SkillTestResults_` and check the token value, or `jq '.gameModifiers'` on the output. If it's missing, the rewind is unusable for that bug; either undo far enough to re-take the choice that *creates* the modifier, or verify with a spec instead. Say so explicitly rather than claiming a no-op diff is a pass.

Related: [[project_replay_undo_entity_token_fidelity]] (the sibling fidelity gap for removed entities), [[project_canresolvetoken_replaces_value_only]].
