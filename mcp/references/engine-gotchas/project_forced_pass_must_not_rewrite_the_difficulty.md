# A forced pass must not rewrite the difficulty

Only an **automatic** success has a total difficulty of 0 (RR / Grimoire *Automatic
Failure/Success*). A forced `SucceededBy NonAutomatic n` — Scrape By (1) `12082`, Lab Coat (1)
`09050`, both via `passSkillTestBy 0` — is **not** automatic, so the test keeps its real
difficulty.

`SkillTest/Runner.hs` `PassSkillTestBy n` used to fake the result by rewriting the test:

```haskell
push $ SkillTestResults $ SkillTestResultsData n 0 0 0 … True
pure $ s & (resultL .~ SucceededBy NonAutomatic n)
         & (difficultyL .~ SkillTestDifficulty (Fixed 0))
         & (difficultyIncreaseL .~ 0)
```

`getModifiedSkillTestDifficulty` folds modifiers over that zeroed base, so `stmModifiedDifficulty`
and `stmValueBreakdown` both reported 0 — the big difficulty number in `SkillTest.vue` — while the
fabricated results data drove the skill value to 0. Symptom (#5775): Agnes at shroud 5 with a
Cultist token, panel read `0 VS 0`.

The zeroing bought nothing: `PassedSkillTest … n` at ST.7 reads `skillTestResult`, not the
difficulty, and a `RecalculateSkillTestResults` would have rebuilt from
`calculateSkillTestResultsData` and printed "succeeded by <modified skill value>" anyway.

Fix: keep the difficulty, publish the real tested values with `skillTestResultsSuccess = True`
(via a new `calculateRawSkillTestResultsData`, which skips the
`AutomaticallyFailIfSucceedByAtLeast` short-circuit), and set a new `skillTestResultForced :: Bool`
on `SkillTest`. `RecalculateSkillTestResultsCanChangeAutomatic` returns early when it is set —
otherwise the restored difficulty flips the forced success into a failure. The frontend gates on
the same flag, reading the succeeded-by amount from `skillTest.result.contents[1]`, so every
non-forced test renders exactly as before.

**Why:** the engine's stored difficulty is the *test's* difficulty, not a scratch variable for
making a result come out right. Lying in it leaks into every consumer — display, breakdown,
recalculation.

**How to apply:** when an effect overrides a computed outcome, mark the override and teach the
recalculation path to respect it; don't back-solve the inputs. Same lesson, opposite direction,
as [[project_auto_success_difficulty_is_zero_not_just_base]] (there the rule really does say the
difficulty is 0, so the *accessor* short-circuits rather than the field being zeroed).

Debug exports carry a card's `errata` string — Scrape By's reads "This card's ability should read
'You succeed at that skill test by 0 instead.'", which is where the reporter's wording came from.
