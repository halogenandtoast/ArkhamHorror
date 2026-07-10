module Arkham.Campaign.Campaigns.NightOfTheZealot where

import Arkham.Campaign.Campaigns.NightOfTheZealot.Achievements (runNotzAchievements)
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.NightOfTheZealot.CampaignSteps
import Arkham.Campaigns.NightOfTheZealot.Import

newtype NightOfTheZealot = NightOfTheZealot CampaignAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

instance IsCampaign NightOfTheZealot where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue TheGathering
    TheGathering -> continue TheMidnightMasks
    TheMidnightMasks -> continue TheDevourerBelow
    other -> defaultNextStep other

nightOfTheZealot :: Difficulty -> NightOfTheZealot
nightOfTheZealot = campaign NightOfTheZealot "01" "Night of the Zealot"

instance RunMessage NightOfTheZealot where
  -- Achievement detection first (Return to NOTZ; self-gating, see the
  -- module). Lives on the shared runner so the Return-to campaign inherits it.
  runMessage msg c =
    runQueueT $ campaignI18n $ lift (runNotzAchievements msg) *> case msg of
      CampaignStep PrologueStep -> do
        storyI "prologue"
        nextCampaignStep
        pure c
      _ -> lift $ defaultCampaignRunner msg c
