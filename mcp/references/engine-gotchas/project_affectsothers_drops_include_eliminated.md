---
name: project_affectsothers_drops_include_eliminated
description: affectsOthers/affectsOthersKnown wrap the matcher in InvestigatorIfThen(Known), which Game.hs's includeEliminated does not look inside — an inner IncludeEliminated is silently lost (#5650)
metadata:
  type: project
---

`select` for investigators (`Game.hs`, ~line 1019) pre-filters the candidate list to
uneliminated investigators unless the *top-level* matcher satisfies `includeEliminated`.
That predicate has no case for `InvestigatorIfThen` / `InvestigatorIfThenKnown`, so it
falls to `_ = False`. Since `affectsOthers`/`affectsOthersKnown` wrap everything in
those constructors, `affectsOthersKnown iid (IncludeEliminated Anyone <> …)` quietly
returns **nothing** once the investigators are eliminated — Embezzled Treasure's
"Distribute starting resources" prompt had zero targets at the end-of-game window.

**How to apply:** put `IncludeEliminated` on the **outside** of the wrapper:
`IncludeEliminated $ affectsOthersKnown iid $ IncludeEliminated Anyone <> …`. The outer
one unfilters the list; the inner one keeps the per-id re-select (`matches a m = elem a
<$> select m`) from re-filtering. Not fixed in `Game.hs` on purpose — `includeEliminated
Anyone = True`, so recursing into the branches would make the ~22 `affectsOthers Anyone`
call sites start matching eliminated investigators. Related:
[[project_playability_uses_active_investigator]].
