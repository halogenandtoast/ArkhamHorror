---
name: project_persisted_carddef_snapshots_go_stale
description: "investigatorStartsWith persists a whole CardDef in the save; structural Eq against the live def breaks the moment the card def is edited, silently skipping the starting card (#5611)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 283f4366-024c-47d6-864d-6d665e488368
  modified: 2026-09-05T11:29:01.451Z
---

`investigatorStartsWith` / `investigatorStartsWithInHand` are `[CardDef]` and are
**serialized into the saved game**, not re-resolved from the card registry. `CardDef`
has structural `Eq` over every field, so any later edit to the live def — a new
`cdTags`, a keyword, an errata — makes the persisted snapshot compare unequal, and
`SetupInvestigator`'s deck search silently finds nothing. The card stays in the deck
with no log line.

Concrete case (#5611): `Assets.duke` gained `cdTags = ["dog"]` on 2026-08-07
(`e528cefde7`, for The Dream-Eaters' Barkham Horror Enthusiast). Every EotE game saved
before that date then started *The Heart of Madness* Parts I and II with Duke buried in
Ashcan Pete's deck. Kate Winthrop's Flux Stabilizer was unaffected only because its def
had not changed (and it is `cdPermanent`, which is a second path into play).

Fixed by re-looking-up each def via `lookupCardDef` and matching the deck on
`toCardCode` instead of `CardDef` equality (`Arkham/Investigator/Runner.hs`,
`SetupInvestigator`). The sibling `startsWithInHand` filter in the same handler already
matched on card codes.

**Why:** the debug export is the giveaway — a persisted def prints only its non-default
fields, so a missing `"tags"` key next to a live def that has one is the whole bug.

**How to apply:** never compare a `CardDef` that came out of persisted state (saved
investigator attrs, a queued `Message`, campaign attrs) with `==`; compare card codes.
Other live instances of the same shape worth auditing: `Campaign/Runner.hs` ~195-196
(`RemoveCampaignCardFromDeck`) and `Investigator/Runner/Card.hs` ~538
(`PutCampaignCardIntoPlay`). Related: [[project_entity_is_matchers_printing_aware]],
[[project_asset_event_builder_lists_are_manual]].
