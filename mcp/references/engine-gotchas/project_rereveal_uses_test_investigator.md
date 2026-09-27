---
title: project_rereveal_uses_test_investigator
description: "RevealSkillTestChaosTokensAgain forwards its InvestigatorId straight into Will (ResolveChaosToken …), so a cancel-and-re-reveal card must pass the SKILL TEST's investigator, not the ability's controller — otherwise the substituted symbol's effect resolves for the wrong player"
---

`RevealSkillTestChaosTokensAgain iid` (`SkillTest/Runner.hs`) does not look up who owns the test. It pushes

```haskell
Will (ResolveChaosToken drawnChaosToken chaosTokenFace iid)
```

for every token still in `skillTestToResolveChaosTokens`, using the **`iid` you handed it**. The preceding `RevealChaosToken (SkillTestSource sid) iid token` likewise attributes the `#after RevealChaosToken` window to that `iid`.

`ResolveChaosToken _ ElderSign iid` *is* `ElderSignEffect iid` (pattern synonym in `Message.hs`), and every investigator's elder-sign handler matches on `is attrs`. So passing the wrong `iid` silently drops the symbol effect entirely — nothing errors, the token still shows the substituted face, and the *value* still comes out right (`getModifiedChaosTokenValue` reads `skillTestInvestigator`, not this message).

Cards that cancel a token and re-reveal it with a `ChaosTokenFaceModifier` must therefore use the test's investigator:

```haskell
withSkillTest \sid -> do
  push $ ChaosTokenCanceled iid (attrs.ability 1) drawnToken   -- iid = controller, correct here
  ...
  withSkillTestInvestigator \tester -> do
    push $ RevealChaosToken (SkillTestSource sid) tester drawnToken
    push $ RevealSkillTestChaosTokensAgain tester
```

Only three call sites exist: `Asset/Assets/BlessingOfIsis3.hs`, `Asset/Assets/CurseOfAeons3.hs`, and `Investigator/Cards/WendyAdamsParallel.hs`. Wendy's is inside her own elder-sign effect so `attrs.id` is already the tester; the two assets both trigger on `RevealChaosToken #cancel Anyone #…` and are explicitly usable on **another** investigator's test at your location — that's where the mismatch bites.

**Why:** Sister Mary used Blessing of Isis (3) on Silas Marsh's Fight test. The second [bless] became an [elder_sign] and used Silas's `+0`, but `ResolveChaosToken … ElderSign "07001"` was pushed for Sister Mary, so Silas was never offered "commit a skill from your discard pile" (#5374).

**How to apply:** any new cancel-and-substitute card — and any other future caller of `RevealSkillTestChaosTokensAgain` — should take the investigator from `withSkillTestInvestigator` / `getSkillTestInvestigator` (`Helpers/SkillTest.hs`) unless it is provably the tester's own effect. Keep `ChaosTokenCanceled` and any "you may …" rider on the controller's `iid`. Related: [[project_canresolvetoken_replaces_value_only]].
