---
name: paycardcost-alone-opens-no-play-windows
description: "A bare PayCardCost message plays a card with no PlayCard #when/#after window and no ResolvedPlayCard — use playCardPayingCost(WithWindows); Untimely Transaction (1) swallowed Shrewd Dealings (#5635)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 6e0b5ceb-6658-4cc4-bc5a-9cab27d198f0
  modified: 2026-09-07T12:18:04.444Z
---

`Window.PlayCard` (`#when` *and* `#after`) and `ResolvedPlayCard` are opened in exactly one
place: `InitiatePlayCardWithWindows` (`Investigator/Runner.hs`). A bare `PayCardCost` message
routes `Do (PayCardCost)` → `createActiveCostForCard … NotPlayAction` → (`ForCard` branch,
`ActiveCost.hs`) → plain `InitiatePlayCard` → `PlayCard … False` → `PutCardIntoPlay`. No window
is ever opened, so **no "when you play a card" reaction can fire off it**, and
`historyPlayedCards` (written only in `Do (PlayCard … True)`) never records the play either.

The engine's correct wrappers live in `Message/Lifted/Card.hs`:
`playCardPayingCost` / `playCardPayingCostWithWindows` = `withTimings (Window.PlayCard iid …) $
payCardCost…`. ~20 cards use them (Joey the Rat, Eon Chart 4, Farsight 4, Salvage 2, …).

**Why:** issue #5635 — Untimely Transaction (1) pushed the raw `PayCardCost otherInvestigator
card …` inside its AskMap label, so Bob playing an Item asset handed over by the event was
invisible to Shrewd Dealings' `freeReaction (PlayCard #when You …)`.

**How to apply:** when a card lets someone play another card, use
`playCardPayingCost(WithWindows)`, never a raw `PayCardCost` — inside a deferred choice,
`capture` the helper per investigator rather than splicing the message. Other sites still
carrying the bare form (same latent gap): `EonChart1.hs`, `Scenario/Runner.hs` (search-and-play),
`Scenario.hs`, `Investigator/Runner/Search.hs`, `Investigator/Runner/Card.hs`,
`JennyBarnesParallel.hs`, and `handlePerformAction` in `Investigator/Runner/Action.hs`
(effectively dead — every `performActionAction` caller passes fight/investigate/evade).
Related: [[skipplaywindows-swallowed-after-window]].
