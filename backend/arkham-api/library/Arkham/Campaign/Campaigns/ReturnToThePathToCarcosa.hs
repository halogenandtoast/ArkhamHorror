module Arkham.Campaign.Campaigns.ReturnToThePathToCarcosa (returnToThePathToCarcosa) where

import Arkham.Campaign.Campaigns.ThePathToCarcosa
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.ThePathToCarcosa.CampaignSteps
import Arkham.Campaigns.ThePathToCarcosa.Helpers

newtype ReturnToThePathToCarcosa = ReturnToThePathToCarcosa ThePathToCarcosa
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

instance IsCampaign ReturnToThePathToCarcosa where
  campaignTokens = campaignTokens @ThePathToCarcosa
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just ReturnToCurtainCall
    ReturnToCurtainCall -> Just (UpgradeDeckStep ReturnToTheLastKing)
    ReturnToTheLastKing -> Just (UpgradeDeckStep ReturnToEchoesOfThePast)
    InterludeStep 1 _ -> Just (UpgradeDeckStep ReturnToEchoesOfThePast)
    ReturnToEchoesOfThePast -> Just (UpgradeDeckStep ReturnToTheUnspeakableOath)
    ReturnToTheUnspeakableOath -> Just (UpgradeDeckStep ReturnToAPhantomOfTruth)
    InterludeStep 2 _ -> Just (UpgradeDeckStep ReturnToAPhantomOfTruth)
    ReturnToAPhantomOfTruth -> Just (UpgradeDeckStep ReturnToThePallidMask)
    ReturnToThePallidMask -> Just (UpgradeDeckStep ReturnToBlackStarsRise)
    ReturnToBlackStarsRise -> Just (UpgradeDeckStep ReturnToDimCarcosa)
    ReturnToDimCarcosa -> Just EpilogueStep
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

returnToThePathToCarcosa :: Difficulty -> ReturnToThePathToCarcosa
returnToThePathToCarcosa =
  campaign
    (ReturnToThePathToCarcosa . ThePathToCarcosa)
    (CampaignId "52")
    "Return to the Path to Carcosa"

instance RunMessage ReturnToThePathToCarcosa where
  runMessage msg c@(ReturnToThePathToCarcosa thePathToCarcosa') = runQueueT $ campaignI18n $ case msg of
    NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
    _ -> ReturnToThePathToCarcosa <$> liftRunMessage msg thePathToCarcosa'
