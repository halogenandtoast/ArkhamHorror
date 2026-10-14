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

This is what blocked Lai Jie (Circus Ex Mortis): "[elder_sign]: +2. If you
succeed, you may choose a skill committed to this test and resolve its effects an
additional time" is High Flying's sentence word for word, and High Flying works.
Her half silently did nothing, because there was nothing recorded yet to repeat.

**Resolved by not repeating anything.** A skill's success effect is a handler on
the test's own notification (`PassedSkillTest` / `FailedSkillTest`), not an
ability — `Deduction` is one line of `RunMessage` — so resolving one an
additional time is telling the card the result again. The `notifyResult` step
pushes `NotifyResultAgain target`, and the elder sign being a late option is
then an advantage rather than the problem: it resolves after every committed
card has heard the result once, which is where a second telling belongs.

Two traps found building it:

* The result must be read when `NotifyResultAgain` RESOLVES, not when it is
  pushed. An option's messages are captured at `PassedThisSkillTestBy`, where
  `skillTestResult` is still `Unrun`, so a step that read the result there
  produced nothing and the option arrived with `"messages": []`.
* A custom skill whose success effect is written as a `silent` forced ability on
  the after-result window cannot be re-notified at all. Escalation was rewritten
  as a `PassedSkillTest` handler; the set's other skills with a success rider
  still want the same conversion.

Nothing static catches it. The def validates, `$repeated` substitutes to a real
`SkillSource`, and `repeatAbilities` reports no bad payload: an empty scan is
indistinguishable from a card whose abilities did nothing.

Anything else that reads what just happened has the same ceiling: an
elder-sign success effect cannot see the result window's abilities, their uses,
or whatever they placed.
