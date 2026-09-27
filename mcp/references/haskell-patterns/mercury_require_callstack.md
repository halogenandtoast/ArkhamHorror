---
title: mercury_require_callstack
description: "`RequireCallStack` is a constraint GHC will NOT auto-solve, so a throwing function must either propagate it or explicitly discharge it with `provideCallStack` — turning \"where did this come from\" into a type-level obligation"
---

**Source:** `local-packages/a-mercury-prelude/src/A/MercuryPrelude/RequireCallStack.hs`
(wraps the `require-callstack` package), `.cursor/rules/exception_handling.mdc`.

`HasCallStack` is solved silently by GHC, so it stops at whichever function forgot to
declare it and the stack you get is useless. `RequireCallStack` is a plain class with no
global instance:

```haskell
error            :: RequireCallStack => String -> a
throwWithCallStack :: (RequireCallStack, MonadThrow m, Exception e) => e -> m a
```

A function calling `error` now has exactly two options — put `RequireCallStack` in its
own context (propagating the obligation to *its* callers), or `provideCallStack` to
discharge it, which is an explicit, greppable admission that the trace ends here.
`errorNoCallStack` exists as the documented escape hatch.

The rollout matters as much as the trick: they shipped it by redefining `error` and
`throwWithCallStack` in the custom prelude rather than by editing every call site, so
the constraint spread as modules were touched.

**How to apply here:** `Arkham.Prelude` is the equivalent chokepoint. The engine has
~900 bare `error` calls; a production 500 currently names the message but not the card
or window that produced it. Re-exporting a `RequireCallStack`-constrained `error` from
`Arkham.Prelude` would make new ones self-documenting, and the existing ones can be
ratcheted down ([[mercury_hlint_ratchet_migration]]).
Related: [[mercury_exception_design]], [[mercury_custom_prelude_discipline]].
