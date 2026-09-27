---
title: mercury_display_vs_show
description: "A `Display` class for human-facing text, kept strictly separate from `Show` (Haskell syntax, ideally `stock`-derived), in a package that depends on nothing so instances flow the right way"
---

**Source:** `local-packages/mercury-display/README.md`.

Two rules, one about types and one about packages.

**The type rule:** `Show`/`Read` are counterparts — `show` emits Haskell syntax that
`read` parses back — and should be `deriving stock`. So `show userId` is
`UserKey {unUserKey = "0000..."}`, which is correct and useless in a sentence. Anything
a human reads goes through `display`. Once the split exists, a `Show` instance can never
be "improved" into prose and break a debug dump, and prose can never leak a constructor name.

**The dependency rule** (stated as a general practice for any class-providing package):

> As a type class package, we should depend on *as few internal packages as possible*.
> Internal packages *should not* be depended upon in order to provide instances.
> Instead, those internal packages should depend on this one to provide those instances.

That inversion is what keeps a shared class from becoming a hub that transitively drags
in the world — the same reasoning behind `mercury-slack-class` and `mercury-settings`
being separate from their implementations.

**How to apply here:** the i18n layer is our `Display` — player-facing strings come from
translation keys, and `Show` output leaking into the UI is a real bug shape. The
dependency-direction rule is the more actionable half: `Arkham.Prelude` and the class
modules under `Arkham/Classes/` should not gain imports in order to provide instances.
Related: [[mercury_custom_prelude_discipline]], [[mercury_local_packages_split]].
