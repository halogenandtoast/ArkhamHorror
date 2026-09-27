---
title: mercury_custom_prelude_discipline
description: "A custom prelude that imports nothing from the app it serves, is named `A.MercuryPrelude` so it sorts first past fourmolu, and is used as the chokepoint for shipping codebase-wide changes"
---

**Source:** `local-packages/a-mercury-prelude/src/A/MercuryPrelude.hs` (read its module
Haddock — it explains every decision).

Three non-obvious decisions:

1. **The prelude must not import anything from the main package.** If it did, the
   modules it imports couldn't use it, and the guarantee "every module in the app uses
   this prelude" would break. It re-exports external packages and thin local submodules
   only.
2. **The `A.` prefix is a formatter workaround, and they say so.** `fourmolu` sorts
   imports into one alphabetised block; GHC reports the *later* of two overlapping
   imports as redundant. A prelude named `MercuryPrelude` sorts after `Data.Text` and
   gets flagged redundant; `A.MercuryPrelude` sorts first. Ugly, documented, correct.
3. **The prelude is the migration lever.** They wanted `annotated-exception`'s
   `throwWithCallStack` without a repo-wide diff, so the prelude exports a compatibility
   shim; same trick for `error` gaining `RequireCallStack`
   ([[mercury_require_callstack]]). A behaviour change lands in one file and spreads as
   modules are touched.

Submodules are split by topic (`Exception`, `Concurrent`, `MonadTime`, `Data.Foldable`,
`Data.Traversable`, `Utilities`, `Cascade`) and re-exported with section headers, so the
prelude stays readable and pieces can be extracted later.

**How to apply here:** `Arkham.Prelude` already plays this role (ClassyPrelude + lens
operators, `NoImplicitPrelude` on). Point 3 is the one to remember: any engine-wide
convention change — a safer `error`, a banned partial function, a new default — is a
one-file change there plus an hlint ratchet, not a mass edit.
Related: [[mercury_hlint_ratchet_migration]], [[mercury_display_vs_show]].
