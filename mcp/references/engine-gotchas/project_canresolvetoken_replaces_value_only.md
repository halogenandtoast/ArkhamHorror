---
title: project_canresolvetoken_replaces_value_only
description: "CanResolveToken only swaps ResolveChaosToken (the token's VALUE); the symbol's other effects live in scenario/investigator PassedSkillTest handlers and keep firing — suppress with IgnoreChaosTokenSymbolEffects"
---

`CanResolveToken face target` (Modifier.hs) makes `Will (ResolveChaosToken …)` in `Game/Runner.hs` offer "resolve with <target>" and push `TargetResolveChaosToken` **instead of** `ResolveChaosToken`. That only replaces the token's **numeric** contribution. A symbol's *other* effects are implemented elsewhere and are not touched:

- **Scenarios** (~100 files under `Scenario/Scenarios/`) handle `PassedSkillTest`/`FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _`. `SkillTest/Runner.hs` builds `tokenSubscribers` from `skillTestRevealedChaosTokens` and dispatches these regardless of how the token was resolved.
- **Investigator elder signs** run through the same path — `Script.passedWithElderSign` matches `PassedSkillTest … (ChaosTokenTarget (chaosTokenFace -> ElderSign))`.

Suppress both with **`IgnoreChaosTokenSymbolEffects`** on the token, scoped to the skill test:

```haskell
skillTestModifiers sid attrs token
  [ChangeChaosTokenModifier (NegativeModifier 1), IgnoreChaosTokenSymbolEffects]
```

Do **not** reuse `IgnoreChaosTokenEffects` for a *replacement* effect: `getModifiedChaosTokenValue` (`Helpers/SkillTest.hs`) maps it to `NoModifier`, so it fights your own `ChangeChaosTokenModifier` in an order-dependent `foldr`. `IgnoreChaosTokenEffects` = ignore value *and* symbol; `IgnoreChaosTokenModifier` = value only; `IgnoreChaosTokenSymbolEffects` = symbol only.

Both gates read the shared predicate `chaosTokenSymbolEffectsIgnored` (`Helpers/ChaosToken.hs`), checked in `Arkham/Scenario.hs`'s Passed/Failed `ChaosTokenTarget` branches and at the `RunMessage Investigator` chokepoint (`Investigator/Runner.hs`) — safe there because `InvestigatorAttrs` only matches `InvestigatorTarget` for those messages.

**Why:** The Black Cat (5) says "instead of that symbol's normal effects", but on The Vanishing of Elina Harper (Hard) an [elder_thing] resolved through it still ran the scenario's "place 1 of your clues on your location", cancelling the clue the investigation had just discovered (#5352).

**How to apply:** any new card using `CanResolveToken` needs the suppression too — if a second one appears, hoist the grant into `Game/Runner.hs` where the replacement choice is made. Related: [[project_replay_undo_drops_skilltest_modifiers]].
