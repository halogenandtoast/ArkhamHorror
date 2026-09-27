---
title: project_replacecard_registry_vs_message
description: "Arkham.Card's replaceCard is the CardGen registry mutation only — it never updates hand/deck/discard/underneath; push the ReplaceCard message when zones matter (#5357)"
---

Two different things share the name, and picking the wrong one half-swaps a card:

- `replaceCard :: CardId -> Card -> m ()` — a **`CardGen` class method** (`library/Arkham/Card.hs`, instance in `GameEnv.hs`). It only does `gameCards = insertMap cardId card` on the IORef. Nothing else moves.
- `ReplaceCard cardId card` — the **message**. `Game/Runner.hs` calls the registry mutation *and* sets `cardsL`; `handleReplaceCard` (`Investigator/Runner/Card.hs`) maps the swap over `handL`, `discardL`, `deckL`, `cardsUnderneathL`, `foundCardsL`, `bondedCardsL`, `decksL`.

The registry-only version is enough for `fetchCard`/`getCard` by id (so `Do (PlayCard ...)`, which refetches, plays the *new* card and everything looks right), but `preloadHandEntities` filters `investigatorHand`, so the in-hand effect entity is still built from the **old** card. Result in #5357: The Painted World played the chosen event correctly, yet Intel Report / Decoy never offered their in-hand `{reaction}` cost-increase abilities at the play window — the entity was still The Painted World.

**How to apply:** use the bare `replaceCard` only for registry fixups on cards that are not in a zone anyone reads (Courage restoring its entry after RemovedFromPlay; GraveLight/FalseColors, which shuffle the new card object in explicitly). If the card sits in hand — or anywhere `preloadEntities` or a zone field reads — `push $ ReplaceCard cid card`. `runQueueT` preserves push order and `preloadEntities` runs before every message, so a `ReplaceCard` pushed ahead of a `checkWindows` is visible to that window. Related: [[project_window_entry_tick_timing]], [[project_truemagick_signmagick_ability_exposure]].
