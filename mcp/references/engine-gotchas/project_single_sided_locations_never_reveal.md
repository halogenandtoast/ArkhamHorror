---
title: project_single_sided_locations_never_reveal
description: Single-sided locations enter play already revealed, so no RevealLocation window ever opens for them — "after this location enters play" must use LocationEntersPlay #after
---

`LocationAttrs` initialises `locationRevealed = not (cdDoubleSided def)` (`Arkham/Location/Types.hs:313`). Every def helper that clears `cdDoubleSided` — `singleSided`, `otherSideIs`, `storyOnBack` / `storyOnBack'`, `singleSidedWithBack` (`Arkham/Location/CardDefs/Base.hs`) — therefore produces a location that is **already revealed the instant it is created**.

Nothing pushes a `RevealLocation` for such a location, and both `RevealLocation` handlers in `Arkham/Location/Runner.hs:420,430` are guarded by `not locationRevealed`, so even a hand-injected one is a no-op. A `forced $ RevealLocation #after Anyone (be a)` ability on a single-sided location is **dead code that fails silently** — it compiles, the ability is listed by `extendRevealed` (the revealed gate *is* satisfied), and it simply never triggers.

The window that does open on entry is `LocationEntersPlay`, pushed from the `PlacedLocation` handler at `Arkham/Location/Runner.hs:397`:

```haskell
mkAbility a 1 $ forced $ LocationEntersPlay #after (be a)
```

Read the printed text carefully — "after this location **enters play**" and "after this location is **revealed**" are different windows, and for a single-sided location only the first one can ever fire. `SnakePit.hs:27` (Return to The Doom of Eztli) and `HiddenVault.hs` (The Apiary) are the reference implementations.

Found via #5538: Hidden Vault [tdc_rune_u] (11579) is `storyOnBack' "11579b"`, so its Forced "search the encounter deck and discard pile for an enemy and spawn it here" never fired — players got the location with no enemy. Verified with `arkham-replay --trace`: `PlacedLocation` → `CheckWindows [After LocationEntersPlay …]` and then nothing, no `FindEncounterCard`.

Related: [[project_flip_this_card_back_over_not_rearm]], [[project_group_reveal_location_windows]].
