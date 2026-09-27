---
name: project-modifier-list-duplicate-copies
description: "Modifier lists carry one entry per source copy, so code that pattern-matches the shape of a folded modifier list crashes on a second copy of the same card"
metadata: 
  node_type: memory
  type: project
  originSessionId: 71e8b27c-77fe-4f6e-af2e-5082e9a4f063
  modified: 2026-08-09T05:43:31.965Z
---

`getModifiers` returns one entry **per granting source**, not a deduplicated set. Two copies of the
same card in play under the same investigator therefore contribute the same `ModifierType` twice.

Any code that folds modifiers into a map/list and then **pattern-matches on its shape** must
deduplicate first, or the second copy is fatal.

Concretely (#5363, "Binder's Jar Bug?"): The Hierophant • V (3) (`54007`) grants
`SlotCanBe ArcaneSlot AccessorySlot` + the reverse. Moon Pendant (`54012`) grants an extra tarot
slot, so two Hierophants can be in play at once — then
`canHoldMap = {ArcaneSlot: [AccessorySlot, AccessorySlot], ...}`. `RefillSlots`' alternate-slot
branches matched only `[]` and `[other]` and fell through to
`error "not designed to work with more than one yet"` the moment an accessory-slot asset needed the
substitution.

**Why:** the duplication is meaningless — "arcane slots may be used as accessory slots and vice
versa" is idempotent — but the fold preserved it and the consumer read list length as semantics.

**How to apply:** build the map through `toCanHoldMap` / `getCanHoldMap`
(`Arkham/Helpers/Investigator.hs`), which does `Map.map nub`. More generally, when you fold
`[ModifierType]` into a structure, decide explicitly whether you want a multiset (stacking numbers,
e.g. `FewerSlots`) or a set (idempotent flags like `SlotCanBe`), and prefer walking a candidate list
over matching a fixed arity. Related: [[project_hand_size_reductions_stack]] is the opposite case —
there the duplicates genuinely stack.
