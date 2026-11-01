# A standalone game has no campaign entity at all

`newGame`'s mode (`Arkham/Game.hs`) is `These CampaignId ScenarioId`:

```haskell
This cid     -> This $ lookupCampaign cid difficulty          -- full campaign
That sid     -> That $ lookupScenario sid difficulty          -- STANDALONE
These cid sid -> This $ overAttrs (stepL .~ ScenarioStep sid) $ lookupCampaign cid difficulty
```

A standalone is the `That` arm — there is **no campaign entity in the game**, so every case in a
campaign's `RunMessage` is unreachable. Nothing errors; the messages simply fall through
unhandled.

The one that bites universally: **`SetChaosTokensForScenario` is handled only by the campaign**
(`Arkham/Campaign/Runner.hs:195` — `push (SetChaosTokens $ campaignChaosBag $ toAttrs a)`).
`LoadScenario` pushes it unconditionally, so in a standalone the bag stays **empty** rather than
erroring. That is why every standalone-capable scenario carries its own

```haskell
StandaloneSetup -> do
  setChaosTokens $ chaosBagContents attrs.difficulty
  pure s
```

(the idiom in every single-scenario homebrew box, e.g.
`Arkham/Homebrew/TheMasqueOfTheRedDeath/Scenarios/TheMasqueOfTheRedDeath.hs`; official scenarios
spell the list out inline). `StandaloneSetup` is pushed from `LoadScenario` and `RestartScenario`
only `when opts.standalone` (`Arkham/Scenario/Runner.hs`). A scenario with no `StandaloneSetup`
case is not a compile error and not a runtime error — it is a game with no chaos tokens.

Scenario-local campaign-log reads are fine: they return the empty default, which is accepted
practice here (official mid-campaign scenarios like `TheInnsmouthConspiracy/ALightInTheFog.hs`
set only the chaos bag in `StandaloneSetup` and let `getHasRecord` answer `False`).

## The `"standalone"` flag in scenarios.json means "can be played on its own"

`frontend/src/arkham/views/NewCampaign.vue` filters the Standalone/PartialCampaign scenario
picker with `s.show !== false && s.standalone !== false`. The `standalone` clause exists for
Hemlock Vale's four Preludes — the only official entries that set it false. A homebrew box that
plays on its own sets `"standalone": true` (`frontend/homebrew/*/scenarios.json` of the
single-scenario boxes).

Every homebrew *campaign* inherited `"standalone": false` on every scenario from the July 2026
placeholder scaffolds and copied it forward, so the Standalone radio rendered with an **empty
scenario grid** and Start stayed disabled — no error, nothing logged (#5935). Removing the flag
is only correct once the box's scenarios can actually stand alone.

## Which homebrew campaigns can, and what the rest need

- **Return to The Innsmouth Conspiracy** — works. Its eight scenarios are newtypes over the
  official Innsmouth scenarios and delegate (`_ -> ReturnToX <$> liftRunMessage msg inner`); all
  eight officials implement `StandaloneSetup`. Same as the official "Return to" boxes, which also
  have no `StandaloneSetup` of their own. Unblocked in #5935.
- **Dark Matter** — blocked. Scanning resolves in `Campaign.hs` via three `CampaignSpecific`
  handlers (`doScanKey`, `doPlaceFacedownKey`, `doDrawFacedownKey`); 5 of 8 scenarios scan, so a
  standalone announces scans that never resolve.
- **Circus Ex Mortis** — blocked. The `EndRoundWindow` moon-token release lives in `Campaign.hs`,
  and the moon tokens themselves are added by the campaign (3 at the prologue, more at
  interludes); 6 of 8 scenarios touch them. How many moons a standalone of scenario N starts with
  is a content call.
- **Ages Unwound** — blocked. `AddToDiscard _ pc | isDummyDeckCard` reshuffles the DummyDeck in
  `Campaign.hs`; 2 of 7 scenarios use it.
- **Night of Vespers** — dev-gated, scenarios are 30–100-line scaffolds.

The general rule: anything a box implements on its campaign is campaign-only. Before offering a
box's scenarios standalone, grep its `Campaign.hs` for cases that fire *during* a scenario
(`CampaignSpecific`, `EndRoundWindow`, `AddToDiscard`, any window message) — each one is a silent
hole.

To tell which arm a debug export is in, read `campaignData.currentData.gameMode`'s keys: `That`
alone is a standalone, `This` + `That` is a campaign inside a scenario, `This` alone is a
campaign between scenarios (see [[project_arkham_replay_tool]]).
