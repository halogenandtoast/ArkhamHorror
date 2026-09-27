---
name: project_handwith_excludes_asifinhand_cards
description: "HandWith resolves to the literal InvestigatorHand field, so a self-referential `HandWith (HasCard (CardWithId a.cardId))` silently matches nobody for an as-if-in-hand card (Norman's deck top, Backpack) — identify the holder with a.owner instead (#5544)"
metadata: 
  node_type: memory
  type: project
  originSessionId: a593ce32-754c-43d0-be70-f19ebc79c910
  modified: 2026-08-29T01:48:03.598Z
---

`HandWith` is evaluated in `Arkham/Game.hs` as `field InvestigatorHand`, which returns
only the literal hand plus in-hand treacheries/enemies. It does **not** include
as-if-in-hand cards (`AsIfInHand`, `AsIfInHandFor`).

But an as-if-in-hand card whose def carries `cdOutOfPlayEffects = [InHandEffect]` **does**
get an entity: `getAsIfInHandEffectCards` feeds `preloadHandEntities`
(`Game/Runner.hs`), which builds it via `addCardEntityWith iid` with placement
`StillInHand`. So `HasModifiersFor` runs — it just can't find "you" via `HandWith`.

The trap is that the failure is silent and numeric, not an error: on #5544
Join the Caravan (1) identified its holder as
`HandWith (HasCard (CardWithId a.cardId))`, so when Norman Withers played it off the
top of his deck `DifferentClassAmong` saw an empty investigator set **and** an empty
card set and returned `0`. The card cost 5 − 0 − 1 = 4 instead of 5 − 2 − 1 = 2, while
an identical copy sitting in another player's real hand computed `3` correctly.

**Why:** "for each X you control" cards are naturally written as "the investigator whose
hand holds me", and that reads as correct until someone plays the card from somewhere
that is only *as if* in hand — Norman Withers, Backpack, Stick to the Plan, Ultimatum of
Chaos. Do not widen `HandWith` to fix this: ~30 call sites use it for hand-size,
discard-from-hand and commit checks where a deck-top card must **not** count.

**How to apply:** in a card's own `HasModifiersFor`, resolve "you" from the entity's
owner, never from hand membership. `EventAttrs`/`SkillAttrs`/`AssetAttrs` all expose
`.owner`, and `addCardEntityWith` sets it to the holder for preloaded hand/discard
entities:

```haskell
n <- calculate (DifferentClassAmong (InvestigatorWithId a.owner) $ cardControlledBy a.owner)
```

`cardControlledBy` (`Matcher.hs`) is `ControlledBy . InvestigatorWithId`.
`Skill/Cards/StrengthInNumbers1.hs` is the reference implementation of this shape.

Related: [[project_asifinhand_suppresses_initiateplaycard]],
[[project_card_leaves_zone_on_cardisenteringplay]] (the other Norman-plays-from-deck-top
trap), [[project_cardcostsource_playability_performer]].
