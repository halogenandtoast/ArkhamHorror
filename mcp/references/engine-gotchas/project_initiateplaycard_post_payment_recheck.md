---
title: initiateplaycard-post-payment-recheck
description: "InitiatePlayCard arrives post-payment from reaction/ability plays; re-checking getIsPlayable with UnpaidCost spuriously fails — credit payment back inline (totalResourcePayment + ReduceCostOf), don't switch to PaidCost"
---

`InitiatePlayCard` reaches handlers at two timings: pre-payment (normal action play from PlayerWindow, `payment = NoPayment`) and post-payment (reaction/ability plays via `playCardPayingCost` → `PayCardCost` → ActiveCost pushes it at `ActiveCost.hs:1711` with `c.payments`). A handler that re-runs `getIsPlayable ... (UnpaidCost NeedsAction)` on the post-payment pass fails affordability (resources already spent) and can silently drop options — Luke Robinson force-played Winds of Power at a connecting location because the current-location branch vanished and `chooseOrRunOneM` auto-ran the lone remaining option (issue #5009).

**Why:** post-payment, the unpaid-cost re-check demands resources/actions the play already consumed; `Helpers/Playable.hs` only skips affordability for `PaidCost`.

**How to apply:** don't blanket-switch to `PaidCost` (user explicitly prefers not to loosen the pre-payment path). Instead adjust inline from the message's `payment` field: `totalResourcePayment payment` (Arkham.Cost) + `withModifiers iid [ReduceCostOf (CardWithId card.id) paid]`, and use `UnpaidCost NoAction` when `payment /= NoPayment` (no phantom #play action). Also: any `doStep`-based "skip re-ask on second pass" tracking must be guarded with `payment == NoPayment` — post-payment plays have no second pass, so the tracked card id goes stale. See LukeRobinson.hs `InitiatePlayCard` handler. Related: [[cardcostsource-playability-performer]].
