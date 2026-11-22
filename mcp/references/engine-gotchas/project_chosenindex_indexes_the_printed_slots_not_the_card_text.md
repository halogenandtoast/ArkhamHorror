# A customization's `ChosenIndex` indexes the PRINTED slots, not the card text

`ChosenIndex n` is produced by exactly one customization in the whole pool —
**Dominance** on Summoned Servitor (09080), *"Summoned Servitor no longer takes up
an (circle one): arcane / ally slot"* — and `n` is an index into the card's
**printed slot list**, not into the card text's reading order.

The authority is the deckbuilder. arkham.build writes this box as its
`remove_slot` choice (`09080.customization_options[5] == {"xp": 2, "choice":
"remove_slot"}` in `mcp/data/cards.json`) and indexes it 0-based against the
card's `real_slot`, which for 09080 is **`"Ally. Arcane"`** — the same order as
the engine's own `cdSlots = [#ally, #arcane]`
(`Asset/Cards/TheScarletKeys.hs`). So `ChosenIndex 0` is **ally** and
`ChosenIndex 1` is **arcane**, while the printed text reads "arcane / ally".

Both engine readers had it backwards —

```haskell
-- SummonedServitor.hs
[DoNotTakeUpSlot $ if 0 `elem` indexes then #arcane else #ally]
-- Helpers/Customization.hs, customizedSlots
Dominance -> filter (/= if 0 `elem` indexes then #arcane else #ally) slots
```

— so a player who circled **ally** had their **arcane** slot freed and the
Servitor still sat in the ally slot (#6016). Nothing errors; the card just frees
the slot you did not pick, which is invisible until someone tests both branches.

`choicesRequired` was the third copy of the same ordering, and it disagreed with
the other two in the *other* direction: `CustomizationIndexChoice ["arcane",
"ally"]` is numbered 0-based by `withIndex`, so a choice made through the
in-engine prompt (`Refine.hs`, `Custom/Steps.hs`) recorded the **opposite** index
from a decklist import. Only one of the two inputs could ever have been right.

Fixed by resolving the index against `cdSlots` in one place —
`dominanceSlot_` / `dominanceSlot` in `Helpers/Customization.hs` — and flipping
the prompt list to `["ally", "arcane"]` so both inputs agree by construction. The
old code also scraped `[i | ChosenIndex i <- concatMap snd (toList
customizations)]` across **every** customization rather than Dominance's own
entry; harmless only because no other box produces a `ChosenIndex`.

Two more traps in the same area:

- `customizedSlots` folds over the customizations, so index against the printed
  `cdSlots def`, never the folded accumulator — another slot-rewriting box
  (`Enchanted`, `ImbuedInk`) would otherwise shift what `n` points at.
- `Decklist.hs`'s `parseIndex` rejects with *"index must be between 1 and 4"*
  while actually accepting `0..4`. The message is wrong, not the guard — do not
  "fix" it into a 1-based reading.

The export tell: the asset's own `slots` still lists both
(`["AllySlot","ArcaneSlot"]` — that field is the printed set), and the
disagreement shows up on the **investigator**, whose `slots` map records the
asset under the wrong slot type with the intended one left empty.

See also [[project_assetslots_ignored_card_target_suppression]].
