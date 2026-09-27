---
name: project_playability_slot_check_ignores_customizations
description: "The playability slot check reads printed cdSlots, so customizations that move a slot (Imbued Ink, Enchanted, Dominance) were invisible while the card was in hand — use customizedSlots (Arkham.Helpers.Customization), not cdSlots"
metadata: 
  node_type: memory
  type: project
  originSessionId: 2a8c0635-2567-482c-92be-b325191cf9da
  modified: 2026-09-04T08:58:44.879Z
---

`getPlayabilityChecksWithResources` (`Arkham/Helpers/Playable.hs` ~555) used to compare
`cdSlots pcDef` — the card's **printed** slots — against `getPotentialSlots`. A card that
moves its own slot does so through the in-play asset's `HasModifiersFor`
(`DoNotTakeUpSlot` + `AdditionalSlot`), and **no asset entity exists while the card is in
hand**, so the modifier could not participate in the playability gate.

Result (#5605): Living Ink (09079) with **Imbued Ink** ("takes up an arcane slot instead of
a body slot") was unplayable while Straitjacket held the body slot, even with an empty
arcane slot. Same latent bug in Hunter's Armor 09021 (**Enchanted**) and Summoned Servitor
09080 (**Dominance**).

Fix: `customizedSlots :: Card -> [SlotType]` in `Arkham/Helpers/Customization.hs`, used by
`Playable.hs` and by the `WillGoIntoSlot` matcher in `Game.hs`. It keys off the
`Customization` constructor, not the card code — each constructor in `Arkham/Customization.hs`
belongs to exactly one card, so the mapping is unambiguous (unlike the `HasTraits Card`
workaround at `Arkham/Card.hs:365`, which special-cases "09021"/"09022" by hand).

**How to apply:**
- Any check that asks "what slots will this card take" for a card **not yet in play** must
  use `customizedSlots`, never `cdSlots`. The in-play path (`field AssetSlots`,
  `fitsAvailableSlots`, `InvestigatorClearUnusedAssetSlots`) is modifier-aware and was
  already correct.
- The card's `HasModifiersFor` modifiers stay — `assetSlots` is seeded from `cdSlots` and
  **persisted**, so removing them would break in-flight games. Two sources of truth on
  purpose. When adding a new slot-moving customization, update both.
- Still not customization-aware: `CardFillsSlot` / `CardFillsLessSlots`
  (`Arkham/Card.hs:292-293`). Harmless today (all callers match `#item`/`#firearm`), but the
  same trap.
- Verify with `arkham-replay --undo 1` — the saved question is **not** recomputed without it
  ([[project_replay_persisted_question_not_recomputed]]).

Related: [[project_cdslots_silent_default_empty]] (the other slot-data trap),
[[project_card_options_system]].
