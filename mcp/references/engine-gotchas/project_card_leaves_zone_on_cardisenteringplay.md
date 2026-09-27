---
name: project_card_leaves_zone_on_cardisenteringplay
description: "A played card leaves hand/deck/discard on CardIsEnteringPlay, not just CardEnteredPlay — assets push CardEnteredPlay after the #when EnterPlay window (#5543)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 616ae458-b1fb-4d57-9a95-c453a6bda369
  modified: 2026-08-28T23:16:31.912Z
---

`CardEnteredPlay` used to be the only message that stripped a played card out of the
owner's hand/deck/discard (`Arkham/Investigator/Runner.hs`). For **player assets** that
message is pushed *after* the `#when EnterPlay` window (`Arkham/Asset/Runner.hs`, both
`PlaceAsset` and `TakeControlOfAsset` push `[whenEnterMsg, CardEnteredPlay, afterEnterMsg]`),
so any `AssetEntersPlay #when` reaction ran while the card was still in its origin zone.
Events and treacheries were never affected — their `PutCardIntoPlay` branches push
`CardEnteredPlay` first.

Invisible for a card played from hand; fatal with Norman Withers, who plays the top card of
his deck: Laboratory Assistant entered play, then its own `#when` "draw 2" reaction drew
from a deck whose top card was the assistant already in play. `TopCardOfDeckIsRevealed`
(the UI), `TopCardOfDeckIs` matchers and Norman's `ReduceCostOf`/`AsIfInHandFor` modifiers
all pointed at the in-play card.

Underneath it: the play path calls `runGameMessage (PutCardIntoPlay ...)` *directly*
(`Arkham/Game/Runner.hs`) instead of pushing it, so the investigator's own
`PutCardIntoPlay` handler — `handlePutCardIntoPlay`, which does strip the zones — never ran
for a normally-played card. A *pushed* `PutCardIntoPlay` (Sleight of Hand, Forged Permit)
did strip immediately.

**How to apply:** the strip now lives in `removeCardFromZones`
(`Arkham/Investigator/Runner/Card.hs`) and runs for **both** `CardIsEnteringPlay` and
`CardEnteredPlay`. `CardIsEnteringPlay` is pushed by the player-asset branch of
`PutCardIntoPlay` before `#when PlayAsset`, so it is the earliest safe point. Both handlers
ignore the `InvestigatorId` (it is the *controller*, which `PlayUnderControlOf` can make
different from the owner) — every investigator filters their own zones. `Arkham/Asset/Runner.hs`
mirrors the same pair for `cardsUnderneathL`, which covers assets that let you play what is
attached to them via the generic strip (Wooden Sledge, Stick to the Plan (3), Twilight Blade,
Dayana Esperence (3), De Vermis Mysteriis (2)); Backpack/Backpack (2) strip themselves on
`PlayCard`, earlier still. Do not reorder `CardEnteredPlay` relative to the `#when EnterPlay`
window; ~30 cards hook it as a trigger.
The 18 cards whose printed text says "After X enters play" were retimed from
`AssetEntersPlay #when` to `#after` (2026-08-29); the only surviving `#when` users are
Remington Model 1858 ("When ... enters or leaves play") and Red Ruin's objective, both of
which print "When". Still outstanding: `Scenario/Runner.hs`'s
`CardEnteredPlay -> ObtainCard` has the same late timing for set-aside cards; and
`Location/Runner.hs` has no enter-play strip for `cardsUnderneathL` at all (no location
currently grants play-from-underneath). See
[[project_asifinhand_suppresses_initiateplaycard]] and
[[project_transformed_investigator_identity]].
