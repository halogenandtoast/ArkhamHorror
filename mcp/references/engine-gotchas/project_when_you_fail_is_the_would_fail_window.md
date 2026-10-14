---
title: project_when_you_fail_is_the_would_fail_window
description: "When you fail a skill test" is WouldHaveSkillTestResult #when, which opens before the result is applied — the after-result window is too late to cancel anything
---

A card that reads "[reaction] When you fail a skill test: cancel all effects of
the failed test" wants

```haskell
WouldHaveSkillTestResult #when You AnySkillTest (FailureResult AnyValue)
```

which is raised at `Window.WouldFailSkillTest` -- *before* the result is
applied, which is the only place a cancel still bites. Ancient Ankh reaches the
same window to change how much a test failed by.

`SkillTestResult #after` is the wrong window for this: by then the failure has
been applied and `CancelEffects` has already been read.

Getting that wrong leads somewhere worse than nothing. The other way in is a
handler on the `Will (FailedSkillTest ...)` the runner announces while the
table is still looking at the Apply Results button, and it *works* -- but the
runner re-announces that message as the failure resolves, so the ability is
offered again after each use. A custom card written that way asked three times
and spent three supplies on one failed test, and no ability limit stopped it:
`GroupLimit`/`PlayerLimit` compare whole `Ability` records, and each
announcement produced a fresh offer.

Two smaller things that bit on the way:

* `FailedSkillTest_` lives on `SkillTestMessage`, so a handler matching
  `{"field": "tag", "of": "$0"}` against `"FailedSkillTest"` matches nothing --
  `$0`'s own tag is `"SkillTestMessage"`.
* A `fast` ability stood up for a handler to call is also clickable whenever the
  player likes. Gate it (`DuringSkillTest SkillTestWasFailed`) or, better, make
  it the reaction it is printed as.
