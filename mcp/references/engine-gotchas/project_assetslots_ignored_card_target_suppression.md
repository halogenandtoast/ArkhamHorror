---
name: project_assetslots_ignored_card_target_suppression
description: "AssetSlots read only getModifiers aid, so a DoNotTakeUpSlot applied to the CardIdTarget (the only target available before the asset exists) was invisible to the slot-fit check"
metadata:
  type: project
---

`AssetSlots` (`Arkham/Game.hs`, `instance Projection Asset`) used to collect modifiers from the
**asset target only**:

```haskell
mods <- getModifiers aid
if isSpirit || DoNotTakeUpSlots `elem` mods then pure [] else …
```

`fitsAvailableSlots` (`Helpers/Investigator.hs:302`) reads that field, and
`Do (InvestigatorPlayAsset …)` (`Investigator/Runner.hs:1793`) turns a `MissingSlots` result into
the "discard an asset to make room" `chooseOne`. So any slot suppression applied to the
**`CardIdTarget`** was ignored — even though the card target is the *only* target available to a
card that must suppress slots **before the asset entity exists**.

The Raven Quill (09042) is exactly that case. Supernatural Record queues
`[addToHand, PayCardCost, handleTargetChoice]`, so the searched asset enters play and is slotted
*before* `HandleTargetChoice` attaches the quill and the permanent
`guardCustomization a SpectralBinding (DoNotTakeUpSlot <$> …)` in `HasModifiersFor` starts to
apply. `SearchFound` already tried to bridge the gap with
`cardResolutionModifiers attrs attrs x (DoNotTakeUpSlot <$> [minBound ..])` — dead code, because
that modifier lands on `CardIdTarget`. Result: Spectral Binding was ignored and the player was
asked to discard a hand-slot asset (#5796).

Note the two paths already disagreed: `InvestigatorClearUnusedAssetSlots`
(`Investigator/Runner.hs` ~1772) *does* check `hasAnyModifier cardId [DoNotTakeUpSlots, DoNotTakeUpSlot slotType]`
alongside the asset's own.

**How to apply:** `AssetSlots` now unions the card target's modifiers, but **only for
suppression** (`DoNotTakeUpSlot` / `DoNotTakeUpSlots`). `AdditionalSlot` and `TakeUpFewerSlots`
stay asset-only — that is the Hunter's Armor duplication the old `TODO` warned about, since
`HuntersArmor` (Enchanted) and `LivingInk` (Imbued Ink) pair `DoNotTakeUpSlot #body` with
`AdditionalSlot #arcane` and would grow a second arcane slot if the card target were read
wholesale. `getModifiers` is a lookup into the preloaded `gameModifiers` map, so the extra read
costs nothing on this hot field.

Also guard the bridging modifier on the customization: unguarded, it suppresses slots for *every*
Supernatural Record play once the card target is honoured. And push `RefillSlots` after the
attach, so the non-Spectral-Binding path and a resolution window that closes early both settle.

Verify with the issue export: `--undo 100`, answer choice 9 (play the quill) then 0 (pick the
found Forbidden Tome). Before: `ChooseOne` of two `AssetTarget`s and the tome left `Unplaced`.
After: tome `InPlayArea`, quill `AttachedToAsset`, nothing discarded.

Related: [[project_refillslots_ignores_at_location_assets]],
[[project_playability_slot_check_ignores_customizations]], [[project_cdslots_silent_default_empty]].
