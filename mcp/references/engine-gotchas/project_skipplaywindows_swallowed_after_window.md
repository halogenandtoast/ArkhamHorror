---
title: skipplaywindows-swallowed-after-window
description: "cdSkipPlayWindows must suppress the #when PlayCard window ONLY — InitiatePlayCardWithWindows is the engine's sole opener of PlayCard #after, so skipping the whole block killed every \"after you play an event\" reaction on The Painted World (#5355)"
---

`InitiatePlayCardWithWindows` (`Investigator/Runner.hs`) is the **only** place in the engine that
opens a `PlayCard #after` window or emits `ResolvedPlayCard`. Nothing downstream re-opens them —
not `ActiveCost`'s `PayCostFinished ForCard`, not `Game/Runner`'s `PlayCard`/`Do (PlayCard)`, not
`PutCardIntoPlay`.

`cdSkipPlayWindows` has exactly one setter: `thePaintedWorld` (`Event/Cards/ThePathToCarcosa.hs`).
It exists because the card replaces itself in place (`replaceCard`, **same `CardId`**) with the
chosen buried event and then pushes its own `#when` window naming *that* card — the runner-built
`#when` window would name the wrong card. But the branch used to skip the whole `pushAll`, so The
Painted World also lost the `#after` window and `ResolvedPlayCard`.

**Why:** a card whose play never opens `#after` is invisible to every "after you play a card/event"
reaction. Sefina Rousseau transfigured into Marion Tavares (`TransfiguredForm "c11001"`) could
never trigger her once-per-round draw off her own signature card (#5355). It also silently broke
`ShedALight`, which anchors its `PassSkillTest` after `ResolvedPlayCard` so Double, Double still
sees a live skill test — via The Painted World that anchor didn't exist and it fell through to the
immediate-push branch.

**How to apply:** gate only the `#when` entry on the flag; keep `InitiatePlayCard`, `afterPlayCard`
and `ResolvedPlayCard` unconditional:

```haskell
pushAll
  $ [CheckWindows [mkWhen (Window.PlayCard iid $ Window.CardPlay card asAction)] | not (cdSkipPlayWindows (toCardDef card))]
  <> [InitiatePlayCard iid card mtarget payment windows' asAction, afterPlayCard, ResolvedPlayCard iid card]
```

Note the `#after` window necessarily names the **pre-replacement** card (The Painted World), while
the `#when` window names the substituted event — `afterPlayCard` is a pre-built `CheckWindows`
computed at initiation, and re-deriving it after resolution is unsafe because The Painted World has
already applied `RemoveThisFromGame` / `RemoveFromGameInsteadOfDiscard` by then. Harmless in
practice: `Matcher.PlayCard` with `BasicCardMatch` compares the card value directly (The Painted
World is an event), and both `ResolvedPlayCard` consumers key on `c.id`, which `replaceCard`
preserves.

That preserved `CardId` is itself a trap in the other direction: the substituted card inherits The
Painted World's `CardId`, so an in-hand entity matching on its *own* `a.cardId` (Intel Report's
`PlayCard #when You (basic $ CardWithId a.cardId)`) will not match the window's card.

Related: [[initiateplaycard-post-payment-recheck]], [[project_window_entry_tick_timing]].
