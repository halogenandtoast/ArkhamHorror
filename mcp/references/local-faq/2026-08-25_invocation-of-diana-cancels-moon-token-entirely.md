---
title: Invocation of Diana cancels a ☾ token's entire effect
date_added: 2026-08-25
source: internal decision (user instruction) — campaign-specific ruling for the Circus Ex Mortis homebrew campaign
affects:
  - Invocation of Diana
  - Moon token (☾)
  - IgnoreChaosTokenEffects
  - ResolveChaosToken
---

# Invocation of Diana cancels a ☾ token's entire effect

**Q: Invocation of Diana cancels each ☾ token revealed during this test. The ☾
token's printed effect is "seal this token on your investigator card, then
reveal another chaos token." Does cancelling it stop only the sealing, or the
whole thing — including the replacement reveal?**

A: The whole thing. A cancelled ☾ token resolves **no** effect: it is **not**
sealed, and **no** replacement chaos token is revealed. It contributes its
printed modifier of 0 to the skill test and nothing else.

This is a full cancel of the token's effect, the same shape as Defiance's
"cancel the token you revealed" — not a partial cancel that leaves the "reveal
another token" clause behind.

## Affected cards / systems

- Invocation of Diana (`:circus-ex-mortis:242`) — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Skills/InvocationOfDiana.hs`
- Moon token effect — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Tokens.hs` (`SealOnRevealerAndRevealAnother`)
- Engine: `IgnoreChaosTokenEffects` short-circuits `ResolveChaosToken` before the reveal effect runs — `backend/arkham-api/library/Arkham/Scenario/Runner.hs:562`

## Implementation status

- **Invocation of Diana**: ✅ correct as written — a `HasModifiersFor` instance
  applies `IgnoreChaosTokenEffects` to `ChaosTokenFaceTarget MoonToken` for the
  duration of the test, which short-circuits `ResolveChaosToken` before either
  the seal or the follow-up reveal is pushed. The card also announces the
  cancel via `cancelledOrIgnoredCardOrGameEffect`.
- This entry ratifies the existing implementation, so a later pass does not
  "fix" it into a seal-only cancel that still reveals a replacement token.
