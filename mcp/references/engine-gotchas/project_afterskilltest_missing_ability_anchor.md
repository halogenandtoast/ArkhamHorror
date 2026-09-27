---
name: project_afterskilltest_missing_ability_anchor
description: "afterSkillTest crashed when the test's AbilitySource had no ResolvedAbility queued (attrs.ability n used as a bare source label); now falls back to EndSkillTestWindow"
metadata: 
  node_type: memory
  type: project
  originSessionId: a19cf9c5-6204-4055-8a33-26c380261d2e
  modified: 2026-07-30T23:53:51.400Z
---

`afterSkillTest` (`Arkham/Message/Lifted.hs`) anchors its `AfterSkillTestOption` to the
`ResolvedAbility` in the queue whenever the current skill test's source is `AbilitySource s n`.
But a `ResolvedAbility` is only ever queued by an actual `UseAbility` activation — cards that call
`beginSkillTest sid iid (attrs.ability n) …` merely as a **source label** (stories, acts, agendas —
e.g. `Story/Cards/RuinsOfSarkomand.hs`) never produce one, so the anchor is absent and the old hard
`insertAfterMatching` blew up with `error "no matching message"` (#5311, Unrelenting (1)).

**Why:** `insertAfterMatching` fails loud by design; `afterSkillTest` had no fallback, so an
entirely normal card+scenario combination hard-crashed the game.

**How to apply:** `insertAfterMatchingMaybe` is now the `Bool`-returning core; `insertAfterMatching`
still errors for its other callers. `afterSkillTest` falls back to `EndSkillTestWindow` (then
`insertAfterMatchingOrNow`) when the ability/event anchor is missing. When adding new
`insertAfterMatching` anchors, ask whether the anchor is actually guaranteed to be queued — a
`Source` built from `attrs.ability n` does **not** imply the ability was activated. Note also that
`Source` equality is by constructor, so an ability whose `ab.source` was re-pointed (Stairway to
Sarkomand's ability carries `LocationSource`) will never match a `StorySource` lookup.

Repro/verification: `arkham-replay <export>` alone reproduced it — the saved queue already had
`UnfocusChaosTokens, DoStep 1` at the head, so a plain drain crashed. Related:
[[project_cancelenemydefeat_queue_layer]], [[project_removed_entities_cleared]].
