---
title: mercury_typed_config_and_flags
description: "Feature flags as named constants with mandatory Haddocks, a default that is always the kill-switch value, and a stub hook so both branches are testable"
---

**Source:** `.cursor/rules/feature_flags.mdc`, `src/FeatureFlags/`.

```haskell
-- | LaunchDarkly Feature Flag for [business purpose].
-- Flag name: "enableColumnDebitCards"
-- Default: False (disabled — acts as a kill switch)
-- Context: Organization-based (uses orgContextFromEntity)
featureFlagName :: FeatureFlagName
featureFlagName = FeatureFlagName "enableColumnDebitCards"

shouldEnableFeature :: (HasStubs m, MercuryFeatureFlags m, MercuryTelemetry m)
                    => Entity Organization -> m Bool
shouldEnableFeature org =
  withDynStubFrom @FeatureFlagStubs stubFeatureFlagName () $
    boolVariation featureFlagName (orgContextFromEntity org) False
```

Transferable rules:

- **Never a bare string at the call site.** One named constant per flag, whose Haddock
  records purpose, default and scope — so a flag can be traced without a dashboard.
- **The default is always the safe/off value**, making every flag a kill switch by
  construction rather than by convention.
- **Every flag has a stub accessor and setter** registered in one `FeatureFlagStubs`
  record, and the rule is to test *both* branches — flags otherwise silently become
  untested code paths ([[mercury_dynamic_stubs]]).
- **Names describe business intent, not the vendor or implementation** ("Braintrust" →
  "businessUnderstanding") — because implementation-named flags outlive their
  implementation.
- Scope is explicit in the type of the context (per-org / per-user / global).

**How to apply here:** the closest analogues are campaign options, Ultimatums/Boons
(`project_ultimatums_and_boons`), the taboo list, and per-card `cdOptions`
(`project_card_options_system`). The two rules that carry over cleanly: the safe default
is the off value, and an option that can't be exercised in a spec from both sides is an
untested branch.
