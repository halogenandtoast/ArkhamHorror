module Arkham.Campaign.Campaigns.BrethrenOfAsh where

import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.BrethrenOfAsh.CampaignSteps
import Arkham.Campaigns.BrethrenOfAsh.Import

newtype BrethrenOfAsh = BrethrenOfAsh CampaignAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

instance IsCampaign BrethrenOfAsh where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue SpreadingFlames
    SpreadingFlames -> continue SmokeAndMirrors
    SmokeAndMirrors -> continue QueenOfAsh
    other -> defaultNextStep other

brethrenOfAsh :: Difficulty -> BrethrenOfAsh
brethrenOfAsh = campaign BrethrenOfAsh "12" "Brethren of Ash"

instance RunMessage BrethrenOfAsh where
  runMessage msg c = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      storyI "prologue"
      nextCampaignStep
      pure c
    _ -> lift $ defaultCampaignRunner msg c
