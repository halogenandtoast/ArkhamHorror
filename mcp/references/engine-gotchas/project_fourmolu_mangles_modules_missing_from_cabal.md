---
title: project_fourmolu_mangles_modules_missing_from_cabal
description: Running fourmolu on a .hs file not yet listed in the .cabal silently rewrites #label to "# label" and attrs.field to "attrs . field" — the module must reach the cabal (hpack --force) BEFORE it is formatted
---

`fourmolu` reads `default-extensions` out of the `.cabal` file it finds for a
module. A **brand-new module that hpack has not picked up yet is not in the
cabal**, so fourmolu formats it without `OverloadedLabels` or
`OverloadedRecordDot` and rewrites working code into broken code, exit status 0:

* `#after` becomes `# after`, parsed as an operator section
* `attrs.ability 1` becomes `attrs . ability 1`
* `can.manipulate.deck` becomes `can . manipulate . deck`

On a module using `ImportQualifiedPost` it then *fails* with ``Found
`qualified' in postpositive position`` — that failure is the visible symptom,
and it is the lucky case. The silent rewrites are the dangerous half, and
re-running fourmolu after the module reaches the cabal does **not** undo them,
because `# after` is by then syntactically valid.

**How to apply:** run `hpack --force` in `backend/arkham-api` BEFORE formatting
any newly created module, and say so in any subagent brief that creates cards —
this bit two agents independently, and the coordinator, during one scenario
drop. After a batch of new cards, sweep the directory with

    grep -rn "# [a-z]"
    grep -rnE "[a-zA-Z0-9_)] \. [a-z]"

(the second has legitimate hits for real function composition, so read them).
Plain `hpack` no-ops on an unchanged `package.yaml`, hence `--force`; see
[[project_hpack_force_for_new_modules]].
