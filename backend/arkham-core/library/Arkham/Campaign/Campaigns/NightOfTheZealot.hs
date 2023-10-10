module Arkham.Campaign.Campaigns.NightOfTheZealot where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.CampaignStep
import Arkham.Campaigns.NightOfTheZealot.Import
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Query

newtype NightOfTheZealot = NightOfTheZealot CampaignAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

instance IsCampaign NightOfTheZealot where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just TheGathering
    TheGathering -> Just (UpgradeDeckStep TheMidnightMasks)
    TheMidnightMasks -> Just (UpgradeDeckStep TheDevourerBelow)
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

nightOfTheZealot :: Difficulty -> NightOfTheZealot
nightOfTheZealot difficulty =
  campaign
    NightOfTheZealot
    "01"
    "Night of the Zealot"
    difficulty
    (chaosBagContents difficulty)

instance RunMessage NightOfTheZealot where
  runMessage msg c = case msg of
    CampaignStep PrologueStep -> do
      players <- allPlayers
      pushAll [story players prologue, NextCampaignStep Nothing]
      pure c
    _ -> defaultCampaignRunner msg c
