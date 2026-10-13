---
title: project_sourceownedby_matched_controller_not_owner
description: "SourceOwnedBy resolved an asset/event/skill source to its CONTROLLER and never its owner, so Diana Stanley never saw her own Delay the Inevitable attached to someone else (#5845)"
---

`checkSourceOwner` (`Arkham/Helpers/Source.hs`) backs both `SourceOwnedBy` (`creditUser =
False`) and `SourceUsedBy` (`creditUser = True`). Its asset/event/skill branches used to
short-circuit on the controller:

```haskell
AssetSource aid ->
  selectAssetController aid >>= \case
    Just iid' -> elem iid' <$> select whoMatcher
    _ -> pure False                                        -- AssetOwner never read
EventSource eid ->
  selectEventController eid >>= \case
    Just controllerId -> elem controllerId <$> select whoMatcher
    Nothing -> ... toCardOwner <$> fieldMay EventCard eid   -- owner ONLY if no controller
```

Owner and controller are different things
(`mcp/references/rules/glossary/ownership_and_control.md`), and they diverge for the cards
that transfer control — Delay the Inevitable, Solemn Vow, Enchanted Armor, Talisman of
Protection, Shrewd Dealings. `PlaceEvent eid (InPlayArea iid')` / `AttachedToInvestigator
iid'` (`Arkham/Event/Runner.hs`, the `PlaceEvent` clause) rewrites `eventController` and
leaves `eventOwner` alone, so for those cards there is always a controller and the owner
branch was dead code.

Diana Stanley's `[reaction]` says "a card you **own**", scoped `SourceOwnedBy You`. With
her Delay the Inevitable attached to another investigator, the cancel window's source is
`AbilitySource (EventSource eid) 1`, which recurses to `EventSource eid`, resolves to the
*other* investigator, and matched nobody. The combo the two cards are printed for never
worked.

Both are credited now. All five `SourceOwnedBy` consumers read as "your card" — Diana says
"own", Carolyn Fern / Vincent Lee / Surgical Kit (3) / Hypnotic Therapy say "one of your
card effects" — and none of the control-transfer cards heal, so crediting the owner adds
Diana's trigger without double-firing the heal riders.

Two things that look like the bug but aren't:

- **The event is discarded before the window.** `DelayTheInevitable`'s ability pushes
  `toDiscardBy` ahead of `cancelledOrIgnoredCardOrGameEffect`, so the entity is gone from
  `entitiesL . eventsL` by the time the window is checked. It is still reachable:
  `getEventMaybe` (`Arkham/Game/Utils.hs`) falls back to the in-discard and removed entity
  maps, controller and owner intact. FAQ (2.22) requires exactly this.
- **The card lands in the wrong discard.** It doesn't — `toDiscardBy` by the controller
  still routes the card to its *owner's* discard pile, which is why Diana's handler finds
  it via `leftPlayButRetrievable` and can place it beneath her.

Still broken after this fix, separately: **Talisman of Protection** (08116) cancels damage
but never calls `cancelledOrIgnoredCardOrGameEffect`, so it raises no window for Diana at
all.
