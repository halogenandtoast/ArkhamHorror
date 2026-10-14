---
title: project_elder_sign_success_runs_before_the_result_windows_abilities
description: An elder sign's "if you succeed" effect resolves before any ability in the after-result window, so nothing it does can read what another card just did
---

An elder-sign effect's success half resolves **before** every ability in the
after-result window. `ElderSignEffect` registers it through
`onSucceedByEffect sid AnyValue … (tokenSkillTestOption ElderSign …)`
(`Custom/Investigator.hs:98`), which fires on `PassedThisSkillTestBy` and so sits
at the ST.7 result-option prompt — the same list as a location's
`$label.discoverClueAt`. The committed cards' own "if you succeed" abilities,
written as `SkillTestResult #after … SuccessResult`, have not run at that point.

Measured with `repeatAbilities`, which reads `InvestigatorUsedAbilities`:

| where the steps ran | what was recorded |
| --- | --- |
| `_elderSignSuccessSteps` (ST.7 option) | nothing at all |
| a `silent` forced on `SkillTestResult #after` | only the investigator's own use |
| a `[reaction]` on the same window | both of the committed skill's uses |

So the investigator's own forced ability in that window still resolves ahead of a
committed skill's, and the first slot that sees them is the **reaction window
right after the result** — which is where High Flying (an event, "Fast. Play
after you succeed by 3 or more") is played, and which an elder sign cannot reach
without being written as an ability the card does not print.

This is what blocks Lai Jie (Circus Ex Mortis): "[elder_sign]: +2. If you
succeed, you may choose a skill committed to this test and resolve its effects an
additional time" is High Flying's sentence word for word, and High Flying works.
Her half silently does nothing — the prompt appears, a skill can be chosen, and
no effect repeats — because there is nothing recorded yet to repeat.

Nothing static catches it. The def validates, `$repeated` substitutes to a real
`SkillSource`, and `repeatAbilities` reports no bad payload: an empty scan is
indistinguishable from a card whose abilities did nothing.

Anything else that reads what just happened has the same ceiling: an
elder-sign success effect cannot see the result window's abilities, their uses,
or whatever they placed.
