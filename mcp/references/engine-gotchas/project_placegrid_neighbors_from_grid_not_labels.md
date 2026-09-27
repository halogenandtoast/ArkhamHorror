---
name: project_placegrid_neighbors_from_grid_not_labels
description: PlaceGrid must read neighbours from the scenario grid, not from location labels — labels are set by a queued message and lie mid-move
metadata:
  type: project
---

`PlaceGrid` (Arkham/Scenario/Runner.hs) rebuilds a location's `locationDirections`
by looking at the four cells around its new position. That lookup **must** read the
freshly-computed `grid`, not `Matcher.LocationWithLabel (gridLabel pos)`.

Labels are only updated by the `SetLocationLabel` message that `PlaceGrid` *pushes*,
so during the handler every label is one move out of date. That breaks two ways:

- **Self-neighbouring:** a location sliding one cell over still carries its old
  label, so it finds *itself* in the cell it just vacated (the Great Lift).
- **Swaps:** when two locations trade positions (Dark Matter's Electric Nightmare
  "switch two locations with each other" — Psychoanalysis / Library), the swap is two
  sequential `PlaceGrid` messages. After the first one runs, the mover has been
  relabelled to the *second* location's cell, so two locations claim the same label.
  `selectOne` then returns whichever comes first, and if it returns the mover itself
  the self-guard drops it — leaving the swapped pair **disconnected from each other**.

The grid has neither problem: it is a `Map`-like structure updated synchronously in
the same handler, the mover's old cell is already cleared, and the new cell already
holds the mover.

```haskell
let getAdjacent dir = do
      GridLocation _ lid' <- viewGrid (updatePosition pos dir) grid
      guard (lid' /= lid)
      pure lid'
```

Everything that reaches the grid goes through `PlaceGrid`
(`Arkham.Helpers.Message.placeLocationInGrid` emits `Run [PlaceLocation, PlaceGrid]`),
so the grid is authoritative — no scenario places a grid neighbour by label alone.

Related: [[project_canmovewith_requires_connecting_destination]],
[[project_cannotenter_enemy_side_ban]].
