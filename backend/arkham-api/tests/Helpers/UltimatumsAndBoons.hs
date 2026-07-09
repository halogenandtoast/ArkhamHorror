-- | Shared plumbing for "Ultimatums and Boons" specs: select entries on a
-- running test game (the harness builds its game with 'defaultSettings') and,
-- for campaign-only entries, attach a campaign to the standalone harness.
module Helpers.UltimatumsAndBoons (
  module Arkham.UltimatumsAndBoons.Types,
  withUltimatumsAndBoons,
  withUltimatumsAndBoonsDisabled,
  withUltimatums,
  withUltimatumsDisabled,
  withSelections,
  withSelectionsDisabled,
  setUltimatumsAndBoons,
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

-- | Select any mix of ultimatums and boons.
withSelections :: [UltimatumOrBoon] -> TestAppT ()
withSelections xs =
  setUltimatumsAndBoons \s -> s {settingsUltimatumsAndBoons = setFromList xs}

-- | Same selection, but with the runtime kill switch flipped off.
withSelectionsDisabled :: [UltimatumOrBoon] -> TestAppT ()
withSelectionsDisabled xs =
  setUltimatumsAndBoons \s ->
    s
      { settingsUltimatumsAndBoons = setFromList xs
      , settingsUltimatumsAndBoonsEnabled = False
      }

-- Specs deal in Boons/Ultimatums directly; wrap into the union at the storage
-- boundary.
withUltimatumsAndBoons :: [Boon] -> TestAppT ()
withUltimatumsAndBoons = withSelections . map Boon

withUltimatumsAndBoonsDisabled :: [Boon] -> TestAppT ()
withUltimatumsAndBoonsDisabled = withSelectionsDisabled . map Boon

withUltimatums :: [Ultimatum] -> TestAppT ()
withUltimatums = withSelections . map Ultimatum

withUltimatumsDisabled :: [Ultimatum] -> TestAppT ()
withUltimatumsDisabled = withSelectionsDisabled . map Ultimatum

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
