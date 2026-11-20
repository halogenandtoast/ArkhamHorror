---
name: project_a_pooled_chaos_token_add_is_capped_by_the_supply
description: "AddChaosToken for a face with a physical supply used to `error` on a dry pool (bless/curse/frost), killing the game and the save; the cap belongs at RESOLUTION, not at push time, because a would-window can change availability — parallel Rex converts exactly when the pool is empty (#6020)"
metadata:
  node_type: memory
  type: project
---

`chaosTokenFacePool` (`Arkham/Homebrew/Tokens.hs`) is the supply table: bless 10,
curse 10, frost 8, blood 12, plus whatever a homebrew `CustomTokenDef` declares in
`tokenPool`. `SetChaosTokens` fills `chaosBagTokenPool` with `supply - already in
the bag`, and every add of a pooled face moves one token from the pool into the bag.

`AddChaosToken` used to hardcode that list and `error` on three of the four faces:

```haskell
BlessToken -> pure $ Just $ fromMaybe (error "no more bless tokens") $ find ...
CurseToken -> ... (error "no more curse tokens") ...
FrostToken -> ... (error "no more frost tokens") ...
BloodToken -> pure $ find ((== #blood) . (.face)) chaosBagTokenPool  -- no-ops
```

So blood degraded correctly and the other three crashed. It is now driven off
`chaosTokenFacePool`: a face with a supply comes only from the pool and a dry pool
adds nothing, which is also what stops a homebrew face's declared `tokenPool` from
being minted past its supply (the old `_ ->` branch sent every `CustomToken` to
`createChaosToken`, unlimited).

**The two rules are different for an effect and for a cost.** An *effect* adds as
many as it can (bless/curse FAQ 2) — so a no-op is correct and must not be an error.
A *cost* that cannot be paid in full cannot be paid at all (FAQ 3), which is
`canPay`'s job (`Helpers/Cost.hs`, `AddFrostTokenCost`/`AddTokenCost`/
`AddCurseTokenCost`); `ActiveCost` still asserts with `error` if it is reached
anyway, and that assertion is deliberate.

**The cap belongs at resolution, not at push time.** `min n <$>
getRemainingCurseTokens` at the call site reads a number that the would-window is
about to change: a reaction inside the window can return tokens to the pool, remove
tokens from the bag, or replace the add outright. Parallel Rex (`90078`) is the
sharp case — "when you would ... add 2 [curse] tokens to the chaos bag: do the other
instead" is *most* useful when the pool is empty, and `ActiveCost`'s
`AddCurseTokenCost` hand-rolls his prompt precisely in the `x < n` branch. Clamping
`addCurseTokens` (`Message/Lifted/Damage.hs`) to the available count would therefore
delete his trigger: the window has to carry the full intent and the supply has to
bite later.

`addChaosTokens n face` (`Helpers/ChaosBag.hs`) is the one home for the
"as many as it can" clamp for the plain, window-less case — it was six hand-rolled
copies of `min n <$> getRemainingBlessTokens` before. `getAvailableChaosTokenFaces`
is the count behind it and behind `canAddChaosTokenFaces`.

**Why:** issue #6020. The bag was at the 10-curse maximum, so parallel Rex playing
Faustian Bargain (`07028`, `cdAdditionalCost = AddCurseTokenCost 2`) reached
`AddChaosToken #curse` with an empty pool. `canPay` correctly allowed the play (it
counts Rex's clues), and the crash was the only possible outcome — then the save was
unrecoverable, because the export keeps the pending question *and* the queue holding
the crashing message. The engine bug underneath was a separate one
([[project_one_printed_ability_must_be_one_index]]), but every path of this shape was
a latent game-killer: the clamp was a convention each caller had to remember.

**How to apply:** never `pushAll $ replicate n (AddChaosToken face)` for a pooled
face — use `addChaosTokens`. Keep a would-window's token list at the full intent.
If you need "if you cannot add one" semantics, ask `canAddChaosTokenFace`; for a
face with no supply (`ElderThing`, `Skull`) that is always `True`, which is why
Ages Unwound models its own `elderThingSupply`.

Related: [[project_one_printed_ability_must_be_one_index]],
[[project_canresolvetoken_replaces_value_only]],
[[project_extra_chaos_draws_open_a_new_reveal_window]].
