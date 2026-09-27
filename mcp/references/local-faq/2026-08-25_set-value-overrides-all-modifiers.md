---
title: "Set the value" overrides all modifiers; "set the base value" does not
date_added: 2026-08-25
source: GitHub issue #5505
affects:
  - The Skeleton Key (2)
  - Obscuring Fog
  - Lantern
  - Matchbox
---

# "Set the value" overrides all modifiers; "set the base value" does not

**Q: How does The Skeleton Key interact with Obscuring Fog or Lantern?**

A: If an effect "sets" the value of a statistic, that overrides all other modifiers. In
other words, if a 4 shroud location has Obscuring Fog and The Skeleton Key attached to it,
its shroud would still be 1, no matter what, because The Skeleton Key sets its value to 1.
It is worth noting that this answer would be different if it set its "base" value — the
"base" value of something is the value before modifiers are applied, so that would allow it
to be modified afterward. But here The Skeleton Key sets its value to 1, AKA the total value
after all modifications.

## Affected cards / systems

- The Skeleton Key (2) (04270) — `backend/arkham-api/library/Arkham/Asset/Assets/TheSkeletonKey2.hs`
- Shroud computation — `getModifiedShroudValueFor` in `backend/arkham-api/library/Arkham/Location/Runner.hs`
- `SetShroud` / `BaseShroud` — `backend/arkham-api/library/Arkham/Modifier.hs`

## Implementation status

- **Shroud computation**: ✏️ updated. `SetShroud` is now applied *after* every `ShroudModifier`
  (an override of the final value); the new `BaseShroud` modifier replaces the printed value
  before modifiers and is what "X shroud" locations use.
- **The Skeleton Key (2)**: ✅ correct as written — it emits `SetShroud 1`, which now wins over
  Obscuring Fog (+2), Lantern (−1), Matchbox (−1), etc.
- **Locations whose printed shroud is "X"** — Bedroom (Hemlock House 32/33/34/35) and The Western
  Wall's level-based locations — switched from `SetShroud` to `BaseShroud`, so cards like Old
  Compass (2) can still reduce their shroud.
- **Matchbox (10108)**: ✅ correct as written. Its text is "gets −1 shroud", an ordinary modifier
  (`ShroudModifier (-1)`), not a set; the fix above is what makes The Skeleton Key override it.
