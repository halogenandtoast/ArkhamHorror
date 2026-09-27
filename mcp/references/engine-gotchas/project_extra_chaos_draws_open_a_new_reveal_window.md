---
name: project_extra_chaos_draws_open_a_new_reveal_window
description: Every extra chaos-token draw (bless/curse/frost/blood, RevealAnotherChaosToken) opens a brand-new WouldRevealChaosTokens window, so PerWindow reaction limits are vacuous across a single test
metadata:
  type: project
---

`DrawAnotherChaosToken` (`Scenario/Runner.hs` bless/curse/frost/blood, and the
`RevealAnotherChaosToken` modifier) goes `RequestAnotherChaosToken` →
`RequestChaosTokens src iid (Reveal 1) SetAside` (`ChaosBag.hs`), and `RequestChaosTokens`
pushes a **fresh** `checkWhen (WouldRevealChaosTokens source)`. A *new* window means
`ConstantReaction`/`ReactionAbility`'s default `PlayerLimit PerWindow 1` no longer blocks
anything — the reaction is offered again inside the same skill test.

Cards whose text says "before revealing chaos tokens **for this test**" must therefore carry
an explicit `perTest`. Breath of the Sleeper + Ocula Obscura got it in #5477 (Glimpse the
Void's `MultiReveal`); Eyes of the Dreamer was missed and re-fired off a bless in #5684.

**Why:** `getAbilityLimit`'s PerWindow branch intersects `usedAbilityWindows` against the
*currently open* windows (`Helpers/Ability.hs`), so it is deliberately scoped to one window.

**How to apply:** any `WouldRevealChaosTokens` / `WouldRevealChaosToken` reaction that the
card limits to the test needs `perTest` (or `perTestOrAbility`) — never rely on the default.
Verifying such a fix with `arkham-replay --undo 1` will *still* show the bad offer: the saved
`UsedAbility` record keeps the old limit ([[used-ability-record-keeps-original-limit]]), so
rewind past the *first* use and walk forward with `--answers` instead.
