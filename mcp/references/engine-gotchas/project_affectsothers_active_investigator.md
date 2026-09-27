---
title: project_affectsothers_active_investigator
description: "affectsOthers/InvestigatorIfThen resolve \"You\" as the ACTIVE investigator, not the performing iid; use affectsOthersKnown iid when performer may differ"
---

`affectsOthers` (Matcher.hs) builds an `InvestigatorIfThen`, whose evaluator (`Game.hs` ~1525) resolves the `CannotAffectOtherPlayersWithPlayerEffectsExceptDamage` check against `activeInvestigatorIdL` — the **active** investigator — NOT the iid performing the effect.

This breaks when a card is performed by a non-active investigator: e.g. a weakness `Revelation` drawn during the Mythos phase or another player's turn. If the *active* investigator has Self-Centered (`c06035`), the matcher wrongly collapses to "only the active investigator" even though the performer has no such restriction.

**How to apply:** ALWAYS prefer `affectsOthersKnown <iid> ...` over bare `affectsOthers ...` whenever the performing investigator id is known in scope — regardless of whether they'd be the active player (user directive, 2026-06-19). Bare `affectsOthers` should only remain where no concrete performer id is available. `affectsOthersKnown` uses `InvestigatorIfThenKnown iid` which checks the explicit iid; both have type `InvestigatorMatcher` so the swap is type-preserving (just prepend the id arg, keep the `$`/parens). For `select`-style calls there's also `selectAffectsOthers iid` (wraps `withActiveInvestigator iid`), but the directive is to prefer `affectsOthersKnown`. Fixed issue #4852 (At a Crossroads, `AtACrossroads1.hs`): two copies drawn back-to-back; the first's "act" option (`takeActionAsIfTurn`) made the seeker active, so the second copy only offered the seeker. Other cards using bare `affectsOthers` (IllPayYouBack, Guidance) share the same latent assumption but are normally played on the owner's own turn.
