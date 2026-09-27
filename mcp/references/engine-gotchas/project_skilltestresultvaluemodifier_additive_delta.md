---
name: project_skilltestresultvaluemodifier_additive_delta
description: "SkillTestResultValueModifier is an additive delta on the RAW result, folded in only at apply time — \"fails by N instead\" cards must apply (N - current) and push RecalculateSkillTestResults"
metadata: 
  node_type: memory
  type: project
  originSessionId: 6011de19-e787-4056-a60e-8a87805e7f15
  modified: 2026-07-30T22:59:17.146Z
---

`SkillTestResultValueModifier n` does **not** set the margin — it adds to the raw
`skillTestResult` stored on the skill test:

```haskell
FailedBy b m -> FailedBy b (max 0 (m + n))   -- SkillTest/Runner.hs, Helpers/SkillTest.hs
```

Two consequences that have already produced a shipped bug (Ancient Ankh, #5310,
applied `n - 1` instead of `1 - n`, so fail-by-2 became fail-by-3):

1. A card reading "that investigator fails by **1**, instead" must apply the
   **delta** `1 - n`, where `n` is the fail-by amount carried in the
   `WouldFailSkillTest` window — not `n - 1`, and not `1`. The bug scales with
   the margin, so a fail-by-2 repro looks mild while fail-by-4 becomes fail-by-7.
2. The stored `skillTestResult` field stays raw; these modifiers are folded in
   only when the Pass/Fail messages are emitted. To inspect the true margin
   mid-resolution use `getSkillTestResultWithResultModifiers`, and to make the
   **UI** show it push `RecalculateSkillTestResults` (which refreshes
   `skillTestResultsResultModifiers`). Omitting the push is silent: the engine
   applies the right number while the player sees the old one.

Reference implementations that get both right: `Asset/Assets/GrannyOrne.hs`,
`Asset/Assets/SteadyHanded1.hs`. Auto-fail is already handled —
`autoFailSkillTestResultsData` folds the modifier in, and
`RecalculateSkillTestResults` routes `FailedBy Automatic` through it without
resetting the result.

**Why:** the additive-delta semantics read like "set the margin" at the call
site, and nothing type-checks the sign.

**How to apply:** when implementing or reviewing any card that changes a
succeed/fail margin, write the modifier as a delta from the window's current
value and push `RecalculateSkillTestResults`. Related:
[[project_onsucceedby_rider_repeat_skilltest]].
