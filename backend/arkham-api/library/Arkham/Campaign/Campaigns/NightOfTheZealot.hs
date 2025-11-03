module Arkham.Campaign.Campaigns.NightOfTheZealot where

import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.NightOfTheZealot.CampaignSteps
import Arkham.Campaigns.NightOfTheZealot.Import

newtype NightOfTheZealot = NightOfTheZealot CampaignAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

instance IsCampaign NightOfTheZealot where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> Just (ContinueCampaignStep TheGathering)
    TheGathering -> Just (ContinueCampaignStep TheMidnightMasks)
    TheMidnightMasks -> Just (ContinueCampaignStep TheDevourerBelow)
    other -> defaultNextStep other

nightOfTheZealot :: Difficulty -> NightOfTheZealot
nightOfTheZealot = campaign NightOfTheZealot "01" "Night of the Zealot"

instance RunMessage NightOfTheZealot where
  runMessage msg c = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      storyI "prologue"
      nextCampaignStep
      pure c
    _ -> lift $ defaultCampaignRunner msg c
