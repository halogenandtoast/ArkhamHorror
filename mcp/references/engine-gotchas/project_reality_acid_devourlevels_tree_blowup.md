---
title: project-reality-acid-devourlevels-tree-blowup
description: "Reality Acid devourLevels materialized an exponential choice tree into one ~46MB message → 27s parseGame → request timeout → game \"locked\" (issue"
---

RealityAcid (Blob That Ate Everything, `85001`) "-1 + (-4 to -8)" outcome = devour
level 1-5 cards totaling >=5. `devourLevels` built the choice by recursing INSIDE the
`chooseOneM $ targets candidates \c -> ... devourLevels ...` continuation. `targets`/
`targeting` evaluate each candidate's continuation eagerly to collect its messages, so
the whole decision tree (15→14→13→…) was materialized into ONE nested `Ask`.

**Why:** that message hit ~46MB; `server/parseGame` (the DB-row-load re-parse every
request does) took ~27s (vs ~10ms healthy) → request timeout → HTTP 500. Frontend had
already cleared the modal + set `processing=true`, so it sat forever = "locked". The
user thought they were stuck on the earlier "consult again" (Curse+Cultist) modal; the
hang is actually on the *next* modal (the devour outcome).

**How to apply:** never recurse-with-branching inside a `chooseOneM`/`targets`
continuation — it materializes the full tree. Use the `doStep`/`DoStep` lazy re-entry
idiom (see `Treachery/Cards/SplinteredSpace.hs`, `Asset/Assets/Ajax.hs`): each pick
devours one card and pushes `doStep remaining (<marker msg>)`; a `DoStep n (<marker>)`
handler re-invokes the loop, recomputing candidates from game state. `DoStep n msg` has
NO engine-level handler — the inner msg is inert unless an entity pattern-matches it, so
reusing e.g. `Revelation iid (toSource attrs)` as a marker is safe.

Diagnosis tools that nailed it: `arkham-replay --simulate-server --metrics` (surfaced
`server/parseGame` as 85% of time) and `jq '...choiceMessages[0]|tostring|length'` on the
export (46MB queued Ask). Related: [[project_action_diff_snapshot]],
[[project_query_cache_readert_passthrough]].
