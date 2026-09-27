---
title: mercury_cascade_fallback
description: "`Cascade` distinguishes \"this attempt failed, try the next\" from \"stop, we can't unwind what we already did\" — and returns the failures it walked past alongside the outcome"
---

**Source:** `local-packages/a-mercury-prelude/src/A/MercuryPrelude/Cascade.hs`.

For operations tried against several providers in turn (a wire via Wise, then
CurrencyCloud, then a bank partner):

```haskell
data CascadeFailure e c = Stop e | Continue c

data CascadeComplete e a
  = CascadeCompleteAllContinued      -- everything declined
  | CascadeCompleteStopped e         -- hard stop partway through
  | CascadeCompleteSucceeded a

data CascadeResult e c a = CascadeResult
  { cascadeResultFinal     :: CascadeComplete e a
  , cascadeResultContinued :: [c]    -- every attempt we walked past, in order
  }
```

The two ideas:

1. **A failure is not one thing.** Splitting `Stop` from `Continue` in the *type* means
   the fallback loop can't accidentally retry past an unrecoverable partial effect — the
   distinction lives where the failure is produced, not in the loop's judgement.
2. **Keep the discarded failures.** `cascadeResultContinued` means the final report can
   say *why each option declined*, which is the difference between "wire failed" and a
   usable diagnostic.

The module header notes it's written to be extractable as a standalone library, so it
imports nothing Mercury-specific — visible as explicit narrow imports from `base`.

**How to apply here:** the analogue is any "try these in order" resolution in the engine
— spawn-location fallbacks, "nearest location" resolution, replacement effects. The
recorded bug `project_nearest_location_unreachable_fallback` is exactly a cascade whose
"no valid path" case was conflated with "not eligible". Modelling declined-vs-fatal
distinctly, and keeping the list of what was skipped, would make those log themselves.
