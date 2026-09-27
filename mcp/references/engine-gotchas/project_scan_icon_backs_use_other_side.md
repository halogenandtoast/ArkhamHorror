---
name: project_scan_icon_backs_use_other_side
description: "A card whose back is its own b side (Dark Matter scanning backs) needs cdOtherSide, not a customBack meta; that is the field the frontend reads for the back image"
metadata: 
  node_type: memory
  type: project
  originSessionId: 5fb0ba49-333b-4654-8f1f-1bf9e98d9dde
  modified: 2026-08-13T10:59:52.468Z
---

`cdOtherSide = Just (flippedCardCode def.cardCode)` is what makes a card's own `b` image
its back. `CardImage.vue` reads `otherSide` before falling back to
`backs/back_encounter.jpg`; `meta.customBack` only names a file under `backs/`, so it
cannot point at a card image.

Dark Matter's scanning backs (icons printed at the bottom of the `b` side) declare it in
all four `CardDefs` modules: `Locations.hs` via `singleSidedWithFlippedBack`, and
`Stories.hs` / `Assets.hs` / `Enemies.hs` in their own `withScanIcons`. These four helpers
are duplicated, so a fix to one has to be applied to all — Assets and Enemies were missing
it (2026-08-13). `withPrintedIcons` (Strange Moons' Brain Cylinders) is the opposite case:
icons on the front, ordinary encounter back, no `b` image exists.

`cdOtherSide` is not purely cosmetic. Before adding it to an encounter card, check:
`flipCard` only follows it when `cdDoubleSided` (default `False`, so safe);
`excludeBSides` / `Helpers.EncounterSet.hasBSide` only exclude codes literally ending in
`b`; `SingleSidedCard` stops matching; and
`shuffleSetAsideEncounterSetIntoEncounterDeck` **skips** any card with `cdOtherSide`, so a
set containing scanning backs cannot be shuffled back in with it.

Related: [[project_encounter_backed_campaign_story_cards]],
[[project_alternate_printings_distinct_carddefs]]
