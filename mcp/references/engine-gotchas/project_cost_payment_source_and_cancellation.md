---
name: project_cost_payment_source_and_cancellation
description: "Cost payments run under PaymentSource, which EncounterCardSource did not unwrap (Deny Existence couldn't see Idle Hands' damage cost); cancelling any part of a cost means the ability's effect does not resolve (#5545)"
metadata:
  type: project
---

`ActiveCost.payCost` (`ActiveCost.hs`) wraps every cost payment's source:
`let source = PaymentSource c.source`. So an encounter card's own ability cost
("Idle Hands: take 2 damage and discard this: take an additional action") opens
`WouldTakeDamage (PaymentSource (AbilitySource (TreacherySource …) 1)) …`.

Two consequences, both fixed for #5545:

1. **Matchers must unwrap `PaymentSource`.** `Matcher.EncounterCardSource`
   (`Helpers/Source.hs`) had no `PaymentSource` case, so it fell through to
   `False` and Deny Existence (05032/05280) was filtered out of
   `getPlayableCards` — the window opened but no ask appeared.
   `SourceIsPlayerCard`/`SourceIsPlayerCardAbility` and
   `Arkham.Source.isEncounterCardSource` already unwrapped it.
   `ScenarioCardSource` and `SourceIsScenarioCardEffect` still do **not** — same
   latent gap. The `notPlayerAbilityIndex` guard (#5342, Poltergeist) still
   rejects basic actions after unwrapping.

2. **Ruling: cancel any part of a cost and you don't get the effect.** Deny
   Existence cancels the 2 damage; Idle Hands is still discarded (that part was
   paid) but you do **not** get the additional action. Implemented as
   `CancelCostPayment ActiveCostId` → `activeCostCancelled = True`, with
   `PayCostFinished`'s `ForAbility` branch gating `UseCardAbility` on
   `not c.cancelled` (mirrors how `ForCard` gates `InitiatePlayCard`).
   `Helpers/Cost.cancelCostPaymentFrom` maps a `PaymentSource` back to its
   active cost via `activeCostSource` (moved to `ActiveCost/Base.hs`).

Do **not** react to bare `CancelDamage`/`CancelHorror` inside `ActiveCost`'s
`runMessage`: costs nest (playing Deny Existence opens its own `ForCard` active
cost), and `activeCostL . traverse` would cancel the canceller too. The existing
`CancelCost` message is different — it deletes the active cost outright, which
strands `BeginAction` without `FinishAction` for action abilities.

Related: [[project_cardcostsource_playability_performer]],
[[project_basic_attack_enemy_source]], [[project_location_issource_matches_any_ability]].
