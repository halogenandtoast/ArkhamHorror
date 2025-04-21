module Arkham.Campaign.Campaigns.NightOfTheZealot where

import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.NightOfTheZealot.CampaignSteps
import Arkham.Campaigns.NightOfTheZealot.Import

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
  campaign NightOfTheZealot "01" "Night of the Zealot" difficulty (chaosBagContents difficulty)

instance RunMessage NightOfTheZealot where
  runMessage msg c = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      storyI "prologue"
      nextCampaignStep
      pure c
    _ -> lift $ defaultCampaignRunner msg c
