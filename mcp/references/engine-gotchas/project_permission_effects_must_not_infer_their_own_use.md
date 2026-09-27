---
name: project_permission_effects_must_not_infer_their_own_use
description: "A play-from-discard permission modelled as CanPlayTopmostOfDiscard + inference fires on any other effect's play of that card; Double, Double replaying a just-discarded event burned Boon of the Child (#5768). Use the Eldritch Tongue shape instead"
metadata:
  node_type: memory
  type: project
  originSessionId: 640ad267-a0c9-4d02-8bbb-a92d6123d239
  modified: 2026-09-25T01:43:55.110Z
---

Boon of the Child ("once per round, an investigator may play the topmost event in their
discard pile") was modelled as a permission plus inference: `CanPlayTopmostOfDiscard`
made the card playable, a `HasModifiersFor` arm bottom-decked any event with
`attrs.playedFromDiscard`, and `CardEnteredPlay` spent the once-per-round marker when the
played card *was the topmost event of the discard*.

Nothing in "a card is sitting on top of the discard" says the boon was what played it.
**Double, Double replays an event that its own first resolution just discarded**, so that
event is by definition the topmost event; `Game/Runner.hs`'s zone heuristic (hand → deck →
discard, decided purely from where the card physically sits) also called the replay
`Zone.FromDiscard`. Result (#5768): Ace in the Hole went to the bottom of the deck and the
boon's use was spent without the player ever invoking it. De Vermis Mysteriis (2), Wendy's
Amulet and Marion Tavares are the same shape.

**How to apply:** model "play a card out of the discard as if it were in your hand, with
riders" the way `Asset/Assets/EldritchTongue.hs` does — the player initiates (ability or
`EffectAction`), and the handler attaches the riders to *that one play* via
`cardResolutionModifiers card source card [...]`, with `AdditionalCost (UnlessFastActionCost 1)`
so a non-fast card still costs an action. Boon of the Child is now a `groupLimit PerRound`
`fastAbility` in `UltimatumsAndBoons.hs` whose criteria and handler share one selector
(`TopmostOfDiscardOf Who CardMatcher`, added to `ExtendedCardMatcher`). The ability limit
replaces the hand-rolled marker.

Still true and separately wrong: `Game/Runner.hs`'s zone heuristic means a Double, Double
replay of Improvised Weapon / Impromptu Barrier / Winging It picks up their "played from
your discard pile" riders. Related: [[project_paycardcost_alone_opens_no_play_windows]],
[[project_playable_card_matchers_fabricate_during_turn]].
