---
name: project_cdslots_silent_default_empty
description: "cdSlots defaults to mempty with no validation, so a CardDef that forgets its slot silently occupies nothing — and any DoNotTakeUpSlot modifier for it becomes a no-op; audit CardDefs against cards.json real_slot"
metadata: 
  node_type: memory
  type: project
  originSessionId: 9b154f11-e271-44b4-a8fb-cc1a0f364414
  modified: 2026-07-30T03:16:52.958Z
---

`cdSlots` defaults to `mempty` (`Arkham/Card/CardDef.hs:395`) and `AssetAttrs` seeds
`assetSlots = cdSlots cardDef` at creation (`Arkham/Asset/Types.hs:488`). Nothing
validates that an `Ally`/`Hand`/`Arcane` card actually declares a slot — `backend/validate`
has no slot check — so an omitted `cdSlots` produces an asset that enters play occupying
**no slot at all**, with no warning anywhere.

Two compounding traps:

1. **A `DoNotTakeUpSlot <slot>` modifier for such a card is dead code.** `Game.hs` (~4719,
   ~5738) implements the printed "does not take up an ally slot" exception by *filtering*
   the asset's own slot list. With `cdSlots = []` there is nothing to filter, so the
   modifier looks implemented and tests as a no-op. This is exactly how #5296 hid: all four
   TCU *Fate* stories (`Story/Cards/{Gavriellas,Jeromes,Pennys,Valentinos}Fate.hs`) had
   correct `DoNotTakeUpSlot AllySlot` effects while the four ally `CardDef`s
   (05258–05261) had no `cdSlots` — so the allies never took a slot in *any* scenario.
2. **`assetSlots` is persisted, not re-derived.** `FromJSON AssetAttrs` reads
   `slots` straight from the saved JSON (`Arkham/Asset/Types.hs:648`), and the
   investigator's slot map is materialised too. Fixing a `CardDef` therefore does **not**
   repair in-flight games — only newly created copies. Say so when closing such an issue.

**Why:** #5296 — a player had 4 allies in play with 3 ally slots (1 base + 2 Charisma);
Penny White (05260) showed `"slots": []` in the debug export.

**How to apply:**
- Audit, don't eyeball. `.claude/data/cards.json` carries `real_slot` per card; compare it
  against each asset `CardDef`'s `cdSlots` (or the `& slot` builder style used in
  `NightOfTheZealot.hs`). Both directions matter, but note the reverse direction is noisy:
  encounter/story-side assets (Maria Rivera 11568, Diving Suit 11764, Pet Oozeling 85030…)
  legitimately have `cdSlots` while arkham.build reports `real_slot: null`.
- When implementing a "does not take up a slot" exception, first confirm the card's own
  `cdSlots` is non-empty, or the modifier silently does nothing.
- Slot regression tests: `self.slots.ally` + `slotItems` (`Arkham/Helpers/Slot.hs`) —
  see `tests/Arkham/Asset/Assets/MitchBrownSpec.hs` and `DaisysToteBagSpec.hs`.
  `putAssetIntoPlay` returns an `AssetId`, not an `Asset` — don't wrap it in `toId`.

Related: [[project_alternate_printings_distinct_carddefs]] (the other class of CardDef
data bug that only an audit against cards.json surfaces),
[[project_takecontrol_no_placeasset]] (`TakeControlOfAsset` does route through
`InvestigatorPlayAsset`, so slot assignment and `DoNotTakeUpSlot` are honored on that path).
