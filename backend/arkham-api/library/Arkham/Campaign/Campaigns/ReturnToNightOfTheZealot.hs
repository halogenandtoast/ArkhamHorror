module Arkham.Campaign.Campaigns.ReturnToNightOfTheZealot where

import Arkham.Campaign.Campaigns.NightOfTheZealot
import Arkham.Campaign.Runner
import Arkham.CampaignStep
import Arkham.Campaigns.NightOfTheZealot.CampaignSteps
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Id
import Arkham.Prelude

newtype ReturnToNightOfTheZealot = ReturnToNightOfTheZealot NightOfTheZealot
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

instance IsCampaign ReturnToNightOfTheZealot where
  campaignTokens = campaignTokens @NightOfTheZealot
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just ReturnToTheGathering
    ReturnToTheGathering -> Just (UpgradeDeckStep ReturnToTheMidnightMasks)
    ReturnToTheMidnightMasks -> Just (UpgradeDeckStep ReturnToTheDevourerBelow)
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

returnToNightOfTheZealot :: Difficulty -> ReturnToNightOfTheZealot
returnToNightOfTheZealot =
  campaign
    (ReturnToNightOfTheZealot . NightOfTheZealot)
    (CampaignId "50")
    "Return to the Night of the Zealot"

instance RunMessage ReturnToNightOfTheZealot where
  runMessage msg c@(ReturnToNightOfTheZealot nightOfTheZealot') = case msg of
    NextCampaignStep _ -> defaultCampaignRunner msg c
    _ -> ReturnToNightOfTheZealot <$> runMessage msg nightOfTheZealot'
