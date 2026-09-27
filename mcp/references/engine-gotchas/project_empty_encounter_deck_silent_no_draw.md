# An exhausted encounter deck used to draw *nothing at all*

`Do (DrawCards iid drawing)` for `Deck.EncounterDeck` (and
`Deck.EncounterDeckByKey`) in `Arkham/Scenario/Runner.hs` has three cases:

- deck has enough cards → `DrewCards`
- deck empty, discard **not** empty → `ShuffleEncounterDiscardBackInByKey` then retry
- deck empty, discard empty → **used to `pure a` and push nothing**

That third case made the draw vanish silently: no `DrewCards`, no window, and any
cards already pulled off a partially-drawn deck (`drawing.alreadyDrawn`) were
dropped on the floor too.

It now pushes `DrewCards iid (finalizeDraw drawing drawing.alreadyDrawn)` — an
empty (or partial) draw report — so scenarios can react.

## Why it matters

Dark Matter's **Lost Quantum** prints:

> When you would draw cards from the encounter deck, if both the encounter deck
> and discard piles are empty, draw a card from the face-down encounter cards in
> your threat area instead. If there are none in your threat area, you are defeated.

`Scenarios/LostQuantum.hs` implements this as

```haskell
DrewCards iid drew | drew.deck == EncounterDeck && null drew.cards -> do
  drewFacedown <- drawRandomFacedownCard iid
  unless drewFacedown $ push $ InvestigatorDefeated (toSource attrs) iid
```

which never fired, because the engine never emitted that message.

## How to apply

- `null drew.cards` on an `EncounterDeck` draw is the "both piles are empty"
  signal — the discard-empty condition is already baked in, since a non-empty
  discard reshuffles and retries instead.
- `CardDrew` carries no requested amount, so a *partial* draw (deck had 1, asked
  for 2) reports the 1 card it got and is indistinguishable from a normal draw.
  The replacement clause only kicks in for a fully-empty draw.
- Cards matching `DrewCards ... | isTarget attrs drewCards.target` on an
  encounter-deck draw can now see an empty `drewCards.cards`; iterate, don't
  pattern-match on a fixed-length list.

See also `project_single_card_shuffle_empty_deck.md`,
`project_discard_check_reshuffle_sweeps_own_cards.md`.
