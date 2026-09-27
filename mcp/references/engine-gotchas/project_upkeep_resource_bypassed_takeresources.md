---
name: project_upkeep_resource_bypassed_takeresources
description: The upkeep "gain 1 resource" step added tokens directly instead of pushing TakeResources, so it opened no GainsResources window
metadata:
  type: project
---

`takeUpkeepResources` (`library/Arkham/Investigator/Runner.hs`) had two branches that
disagreed:

- With `MayChooseNotToTakeUpkeepResources` (Dark Horse), the prompt's label pushed
  `TakeResources iid amount (ResourceSource iid) False` — which routes through
  `handleTakeResourcesV2` → `Do (TakeResources …)` and fires both the `#when` and
  `#after` `GainsResources` windows.
- The ordinary branch did `pure $ a & tokensL %~ addTokens Resource amount`, mutating
  state directly. No `TakeResources`, no `PlaceTokens`, **no window at all**.

So a reaction to "when you gain 1 or more resources" fired on the resource *action*
and on card effects, but never on the upkeep resource — except for a Dark Horse
investigator, who took the other branch. Found while implementing Good Money
(11756a), whose whole engine is banking upkeep resources.

Fixed by making the ordinary branch push the same `TakeResources` message. Side
effects of routing through the real path, all checked and wanted:
`HistoryResourcesGained` now records the upkeep gain (only read via `TurnHistory` by
Obsessed Gambler, and upkeep is outside any turn), and `AdditionalResources`
modifiers apply (only ever granted transiently by Stylish Coat's reaction, whose
window requires `SourceIsPlayerCard <> SourceIsCardEffect` — `ResourceSource iid` is
neither).

**How to apply:** when a card reacts to a resource/damage/clue *gain*, check that
every producer of that gain actually goes through the message that opens the window.
Direct `tokensL %~ addTokens` in a runner is silent. See
[[project_window_entry_tick_timing]].
