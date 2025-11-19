module Arkham.Campaign.Campaigns.StandaloneCampaign (standaloneCampaign) where

import Arkham.Campaign.Import.Lifted

newtype StandaloneCampaign = StandaloneCampaign CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

standaloneCampaign :: Difficulty -> StandaloneCampaign
standaloneCampaign = campaign StandaloneCampaign (CampaignId "00") "Standalone campaign"

instance IsCampaign StandaloneCampaign where
  campaignTokens = const [] -- TODO implement Difficulty -> [ChaosTokenFace]
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Nothing
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    ChooseDecksStep nextStep' -> Just nextStep'
    _ -> Nothing

instance RunMessage StandaloneCampaign where
  runMessage msg c@(StandaloneCampaign _attrs) = runQueueT $ case msg of
    CampaignStep PrologueStep -> do
      pure c
    _ -> lift $ defaultCampaignRunner msg c
