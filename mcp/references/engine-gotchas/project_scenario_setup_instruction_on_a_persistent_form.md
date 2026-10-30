---
title: project_scenario_setup_instruction_on_a_persistent_form
description: "A scenario-only setup instruction implemented on a persistent investigator form (Body of a Yithian) silently fires in every later scenario — it stripped Relic of Ages and bricked Shattered Aeons act 3 (#5929)"
---

`BodyOfAYithian.SetupInvestigator` filtered `#unique <> #item <> #asset` out of the
deck on **every** `SetupInvestigator`. That clause is The City of Archives' own
setup instruction — *"each investigator searches their deck and play area for all
unique Item assets. Set each of those assets aside… They do not exist in this time
period"* — and the scenario already implements it itself at
`Do (CheckWindows [… DrawingStartingHand …])` in
`Scenario/Scenarios/TheForgottenAge/TheCityOfArchives.hs`.

But the Yithian body persists for the rest of the campaign, so the filter kept
firing in The Depths of Yoth, Shattered Aeons and Turn Back Time. Relic of Ages
(`04191`, `cdUnique`, `Item. Relic.`) matched, so Shattered Aeons act 3 ("The
Yithian Relic", `04320` — *"Search your deck or discard pile for the Relic of
Ages"*) had no satisfiable criterion and the scenario became unwinnable (#5929).
It also quietly deleted the player's signature weapon and both Hyperphysical
Shotcasters.

It was added in `c1fa330eea` for a real reason: Charon's Obol (1) (`03308`) is a
unique Item **Permanent**, so it enters play *during* `SetupInvestigator` and
installs `KilledIfDefeated` through `gameModifiers`; the scenario's later removal
takes the asset but the modifier outlives it. Pre-filtering the deck stopped that
— it just wasn't scoped. Fix: `selectOne TheScenario` gated on
`["04237", "53053"]` (City of Archives + Return To, which keeps its own id while
setting `referenceL .~ "04237"`).

**Why:** a persistent form (`YithianForm`, `becomeHomunculus`,
`becomeShatteredSelf` — see [[project_transformed_investigator_identity]]) outlives
the scenario that granted it, so anything the form's `SetupInvestigator` does
happens in every later scenario of the campaign.

**How to apply:** a setup instruction printed in ONE scenario's setup list belongs
in that scenario, never in a persistent investigator form's `SetupInvestigator`.
If setup ordering forces it onto the form (a Permanent entering play before the
scenario's own `DrawingStartingHand` hook), gate it on `selectOne TheScenario`.
Also note `gameModifiers` installed by an asset are NOT retracted when that asset
leaves play — see [[project_inresolution_guard_fails_open_at_endofscenario]] for
the sibling trap.
