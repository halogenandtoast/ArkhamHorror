-- | Shared plumbing for "Ultimatums and Boons" specs: select entries on a
-- running test game (the harness builds its game with 'defaultSettings') and,
-- for campaign-only entries, attach a campaign to the standalone harness.
module Helpers.UltimatumsAndBoons (
  module Arkham.UltimatumsAndBoons.Types,
  withUltimatumsAndBoons,
  withUltimatumsAndBoonsDisabled,
  asCampaign,
) where

import Arkham.Campaign (lookupCampaign)
import Arkham.Difficulty
import Arkham.Game.Settings
import Arkham.UltimatumsAndBoons.Types
import TestImport

setUltimatumsAndBoons :: (Settings -> Settings) -> TestAppT ()
setUltimatumsAndBoons f = do
  overTest \g -> g {gameSettings = f (gameSettings g)}
  -- run a no-op message so preloaded modifiers pick up the new settings
  tick

-- Specs deal in Boons directly; wrap into the union at the storage boundary.
withUltimatumsAndBoons :: [Boon] -> TestAppT ()
withUltimatumsAndBoons bs =
  setUltimatumsAndBoons \s -> s {settingsUltimatumsAndBoons = setFromList (map Boon bs)}

-- | Same selection, but with the runtime kill switch flipped off.
withUltimatumsAndBoonsDisabled :: [Boon] -> TestAppT ()
withUltimatumsAndBoonsDisabled bs =
  setUltimatumsAndBoons \s ->
    s
      { settingsUltimatumsAndBoons = setFromList (map Boon bs)
      , settingsUltimatumsAndBoonsEnabled = False
      }

-- | The test harness runs standalone (@That scenario@); campaign-gated
-- entries (Boon of Persephone, Boon of the Ancients) need a campaign present.
asCampaign :: TestAppT ()
asCampaign = do
  overTest \g ->
    g
      { gameMode =
          These
            (lookupCampaign "01" Easy)
            (fromJustNote "test harness always has a scenario" $ modeScenario (gameMode g))
      }
  tick
