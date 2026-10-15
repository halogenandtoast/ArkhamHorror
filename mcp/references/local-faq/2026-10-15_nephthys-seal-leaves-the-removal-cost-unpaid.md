---
title: Sealing bless tokens on Nephthys leaves a removal cost unpaid
date_added: 2026-10-15
source: rules Q&A relayed by the user (designer answer; venue not recorded — confirm attribution); follow-up to an earlier parallel Zoey / Nephthys ruling
affects:
  - Nephthys (Huntress of Bast) (4) (07262)
  - Zoey Samaras (Parallel) (90059)
  - Zoey's Cross (Advanced)
  - The Key of Solomon (Secrets of the Unknown) (4)
  - ReturnChaosTokensToPoolCost
  - cost payment / replacement effects
---

# Sealing bless tokens on Nephthys leaves a removal cost unpaid

**Q: I have a further question on the parallel Zoey and Nephthys reaction answer. It was
ruled that you can't do the ability if you do the replacement effect. However, does this
make the ability fizzle and do nothing but still allow you to seal on Nephthys, or does
it cancel the ability, refunding the cost and thus putting the blesses back in the bag?**

A:

> If Parallel Zoey triggered her ability to remove 3 bless tokens from the chaos bag
> while she had Nephthys in play, then Nephthys' reaction ability could trigger to seal
> them; this would result in Zoey's ability cost not being paid, and she would not deal
> additional damage for that attack. The tokens would remain sealed on Nephthys.

The ruling this answers ("you can't do the ability if you do the replacement effect") is
not itself recorded in this directory — only this follow-up is. If the original answer
turns up, add it as its own entry and link the two.

So it is the middle outcome, not a refund:

- The tokens **stay sealed on Nephthys**. They do not go back in the bag and they do not
  go to the token pool.
- Zoey's cost was **not paid**, so her ability resolves no effect — no +1 damage on that
  attack.
- Nothing else is given back. This is the general rule that cancelling or ignoring any
  part of a cost means the cost was not paid in full, while what was already paid stays
  paid (see `cancelCostPaymentFrom` in `Arkham/Helpers/Cost.hs`).

This applies to any cost paid by removing bless tokens from the bag, not just Zoey's:
Zoey's Cross (Advanced), The Key of Solomon (Secrets of the Unknown) (4), and the
Innsmouth events with a `ReturnChaosTokensToPoolCost` additional cost.

## Affected cards / systems

- Nephthys (Huntress of Bast) (4) (`07262`) — `backend/arkham-api/library/Arkham/Asset/Assets/NephthysHuntressOfBast4.hs`
- Zoey Samaras (Parallel) (`90059`) ability 2 — `backend/arkham-api/library/Arkham/Investigator/Cards/ZoeySamarasParallel.hs`
- Engine: `ReturnChaosTokensToPoolCost` payment, which batches the chosen tokens into one
  `ReturnChaosTokensToPool` so a single removal window opens —
  `backend/arkham-api/library/Arkham/ActiveCost.hs:697-717`
- Engine: `ReturnChaosTokensToPool` raises
  `TokensWouldBeRemovedFromChaosBag #when` for the tokens that were actually in the bag —
  `backend/arkham-api/library/Arkham/ChaosBag.hs:772`
- Engine: `CancelCostPayment` → `activeCostCancelled`, which `PayCostFinished` reads to
  drop the `UseCardAbility` — `backend/arkham-api/library/Arkham/ActiveCost.hs:1886`, `:1894`

## Implementation status

- **Tokens stay sealed**: ✅ already matched. `ReturnChaosTokensToPool` moves the tokens
  to the token pool and *then* the window resolves, but `SealChaosToken` filters
  `tokenPoolL` as well as the bag, so Nephthys' seal takes them out of the pool and onto
  her sealed list. Net result is correct with no change.
- **Cost not paid**: ✏️ fixed. Nephthys' ability 1 sealed the tokens and stopped there,
  so Zoey's cost looked paid and `PayCostFinished` pushed her `UseCardAbility`, giving
  her the +1 damage anyway. Her handler now walks `getActiveCosts` and pushes
  `CancelCostPayment` for any cost whose payments already record one of the sealed tokens
  (`returnedChaosTokenPayments`, a new prism helper beside `sealChaosTokenPayments` in
  `Arkham/Cost.hs`). The reaction resolves inside the cost payment, well before
  `PayCostFinished`, so the cancel lands in time.
- **Timing note**: the match is on the payment, not on "the only cost in flight", because
  the window carries no source — `ReturnChaosTokensToPool` has none to carry. A removal
  that is not a cost payment (`ResetChaosTokens`, Nephthys' own ability 2) records no
  `ReturnChaosTokensToPoolPayment`, so nothing is cancelled.
- **Not changed**: the `playerLimit PerRound` on Zoey's ability is still spent. A
  cancelled activation is still an activation (same reading as Ire of Shub-Niggurath's
  "Recorded on trigger, not on cancel").
