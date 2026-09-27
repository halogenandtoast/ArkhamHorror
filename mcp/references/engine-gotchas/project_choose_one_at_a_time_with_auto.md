---
name: project_choose_one_at_a_time_with_auto
description: "Pick these N in any order" must use ChooseOneAtATimeWithAuto, not a recursive targets continuation — the recursive form materializes N! branches into one Ask
metadata:
  type: project
---

For "place/resolve these N cards in any order", the flat construction is the only
safe one:

```haskell
(_, choices) <- runChooseT $ targets cards $ putCardOnTopOfDeck lead deck
push $ Msg.chooseOrRunOneAtATimeWithAutoLabel promptLabel autoLabel player choices
```

`Entity.Answer` does the re-asking: index 0 runs every remaining choice ("I don't
care about the order"), any other index resolves that one and re-asks with the
remainder, and it downgrades to a plain `ChooseOneAtATime` once a single option is
left — so the auto choice disappears exactly when it stops being meaningful. The
frontend renders `label` as choice 0 (`Game.ts` `questionChoices`), and the AI
answers 0. All of this already existed and was unused before The Drowned City.

**The trap:** writing the same shortcut by recursing inside the choice —

```haskell
-- WRONG: builds every permutation
targets remaining \card -> do onTop card; go (filter (/= card) remaining)
```

`targets` evaluates each continuation eagerly to collect its messages, so the whole
N! tree lands in one queued `Ask`. Obsidian Canyons' Search the Spires (`To the
Ancient Dome!`, act `11644` ability 1, `ClueCostX`) reveals X cards; at X=7 that is
5040 branches. Measured after switching to the flat form: the ordering question is
**3.4KB** and the full activate→pay→reveal→auto sequence is **267ms** with
`--simulate-server`. The recursive form's message is estimated in the megabytes,
which is the same shape as the Reality Acid incident where a 46MB `Ask` turned
`server/parseGame` into a 27s request timeout.

Still-recursive nearby: `chooseSummitPlacement` (Dazzling Skyline's top-or-bottom
prompt) recurses the same way, but is fixed at 3 cards. Top-vs-bottom is a real
per-card decision, so it needs `doStep` re-entry rather than this helper.

Related: [[project_reality_acid_devourlevels_tree_blowup]],
[[feedback_shallow_message_nesting]]
