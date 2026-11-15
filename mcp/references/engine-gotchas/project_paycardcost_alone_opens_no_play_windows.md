---
name: paycardcost-alone-opens-no-play-windows
description: "A bare PayCardCost message plays a card with no PlayCard #when/#after window and no ResolvedPlayCard — use playCardPayingCost(WithWindows) or playCardPayingCostMessages; swept every site 2026-11-14 (#5635, #5998)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 6e0b5ceb-6658-4cc4-bc5a-9cab27d198f0
  modified: 2026-11-14T10:35:07.548Z
---

`Window.PlayCard` (`#when` *and* `#after`) and `ResolvedPlayCard` are opened in exactly one
place: `InitiatePlayCardWithWindows` (`Investigator/Runner.hs`). A bare `PayCardCost` message
routes `Do (PayCardCost)` → `createActiveCostForCard … NotPlayAction` → (`ForCard` branch,
`ActiveCost.hs`) → plain `InitiatePlayCard` → `PlayCard … False` → `PutCardIntoPlay`. No window
is ever opened, so **no "when you play a card" reaction can fire off it**, and
`historyPlayedCards` (written only in `Do (PlayCard … True)`) never records the play either.

The engine's wrappers:
- `Message/Lifted/Card.hs` — `playCardPayingCost` / `playCardPayingCostWithWindows` =
  `withTimings (Window.PlayCard iid …) $ payCardCost…`. Use these wherever the site is lifted.
- `Helpers/Message.hs` — `playCardPayingCostMessages :: HasGame m => InvestigatorId -> Card ->
  [Window] -> m [Message]`, for a site that still builds a pure `[Message]` for a
  `targetLabel`/`Label`. It goes through the monadic `checkWindows`, not the pure `frame`,
  because `frame` drops the current batch id.

**Why:** issue #5635 — Untimely Transaction (1) pushed the raw `PayCardCost otherInvestigator
card …` inside its AskMap label, so Bob playing an Item asset handed over by the event was
invisible to Shrewd Dealings' `freeReaction (PlayCard #when You …)`. Then #5998 — Dirty Deeds
(3) had the same bug: the asset it searched out and played never offered Shrewd Dealings at all.

**How to apply:** when a card lets someone play another card, never push a bare `PayCardCost`.
As of 2026-11-14 every known site is converted, so a `grep -rn PayCardCost` hit outside
`Game/Runner.hs` (the handler), `Event/Runner.hs` (a matcher) and the two helpers above is a
new regression. The sweep covered `DirtyDeeds3`, `TheRavenQuill`, `EtherealWeaving3`,
`JennyBarnesParallel`, `EonChart1`, `Scenario.hs`, `Scenario/Runner.hs` (×2),
`Investigator/Runner/Search.hs` (`PlayFound`), `Investigator/Runner/Card.hs` (Foresight) and
`Investigator/Runner/Action.hs` (`handlePerformAction`).

Still open, deliberately: `ResolvedPlayCard` remains exclusive to
`InitiatePlayCardWithWindows`, so these plays still don't anchor `ResolvedPlayCard` reactions
(Shed a Light), and `historyPlayedCards` still doesn't record them. `PlayFoundNoCost`
(`Investigator/Runner/Search.hs`) pushes `PutCardIntoPlay` directly and opens no window either
— a different message with its own "put into play vs play" argument.

Related: [[skipplaywindows-swallowed-after-window]],
[[initiateplaycard-post-payment-recheck]].
