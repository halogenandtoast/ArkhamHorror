# Phase 7: Scenario transition flow

## Goal
Between scenarios (resolution -> campaign log -> XP/upgrades -> next scenario setup -> opening draw) the player stops only for: reading story text, campaign choices, deck upgrades, mulligan. Setup the engine already performs (encounter deck build, set-aside cards, starting locations) runs without clicks; round 1 skips Mythos as the rules say (the engine already does this).

## Steps
1. From the phase-1 inventory, list the clicks of the Gathering -> Midnight Masks transition.
2. Add policy rules for trivial setup asks (single-option non-narrative "continue", solo order).
3. Story screens: one click by default; optional "auto-advance story after N s" if approved (unresolved question 2 in plan.md).
4. Reveal feed shows "Encounter deck built: N cards", "Scenario setup complete".

## Done when
Transition clicks = story pages + real choices only.
