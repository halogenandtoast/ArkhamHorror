# Custom chaos bags

Use `Arkham.TokenBag` for auxiliary draws, not the skill-test chaos bag. `BagToken`
contains only a unique ID and face; `CustomChaosBag` is a `TokenBag BagToken`.
Identical faces remain distinct physical tokens.

## Scenario-owned bags

`ScenarioAttrs.scenarioCustomChaosBags` is a map from stable text keys to bags.
Any number of bags can coexist without consuming scenario metadata. Old scenario
saves without this field load an empty map.

Import `Arkham.Helpers.CustomChaosBag` and `Arkham.TokenBag`:

```haskell
-- During setup:
initCustomChaosBag "fury" [Skull, Cultist, Tablet, ElderThing]

-- In a later handler:
bag <- getCustomChaosBag "fury"
(drawn, bag') <- drawBagToken (.face) bag
setCustomChaosBag "fury" bag'
for_ drawn \token -> do
  -- Queue the scenario's reveal windows and effects here.
  focusChaosTokens [asChaosToken token] \unfocus -> do
    -- Queue resolution, then unfocus.
    push unfocus
```

A missing bag is an error: initialize it during setup. `removeCustomChaosBag`
removes one named bag. Set/init/remove operations queue messages; reads do not see
queued writes until those messages resolve. Thread the returned bag through
multi-draw operations and persist once, rather than reading stale state repeatedly.

## Draw lifecycle

- `drawBagToken`: removes one available token, puts it in `currentToken`, and
  consumes a one-shot debug override. Empty bags return `Nothing`; a pending reveal
  is never overwritten.
- `setAsideBagToken`: moves the current token to the set-aside pile.
- `returnBagToken`: returns the current token (e.g. a cancelled reveal).
- `returnSetAsideTokens`: replenishes from set-aside without touching a current reveal.
- `allBagTokens`: lists all three piles.

The owner still controls windows, cancellation, token effects, and replenishment.
Infestation reinitializes after its second cultist is set aside; Predation returns
its existing set-aside tokens after its tablet resolves; Fury draws without replacement
through Moon recursion and then returns all drawn tokens.

Infestation and Predation retain story-owned storage, but share the same types and
operations. Their old bag/token JSON fields are accepted, including Predation saves
without `predationCancelNext`. New writes use only shared fields. Fury has no legacy
format fallback.

## Debugging

- Story bags: open the chaos bag window, select the bag, and enable debug mode
  for its contents controls.
- Scenario bags: enable debug mode and open scenario debug options. All named bags
  appear automatically; adding a bag needs no frontend registration.
- Inspect in-bag, current, and set-aside tokens. In debug mode, click an in-bag token
  to force that face on the next draw once; **Clear** restores random selection.
- Use **Add** to add a standard or registered homebrew token, **−** to remove an
  in-bag token, **↓** to set it aside, **↑** to return one, or **Return all** to
  replenish the set-aside pile. Edits operate on current server state and physical
  token IDs, not a stale client snapshot. Removing the last available copy of a
  forced face clears that override. The current reveal cannot be edited while
  queued effects still refer to it.
- Overrides select an existing token, never manufacture one or bypass normal
  effects. An unavailable/stale override is cleared on drawing. A cancelled test
  that never draws does not consume the override.
