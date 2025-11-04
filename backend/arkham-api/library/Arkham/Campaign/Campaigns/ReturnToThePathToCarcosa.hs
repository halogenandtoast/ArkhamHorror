module Arkham.Campaign.Campaigns.ReturnToThePathToCarcosa (returnToThePathToCarcosa) where

import Arkham.Campaign.Campaigns.ThePathToCarcosa
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.ThePathToCarcosa.CampaignSteps
import Arkham.Campaigns.ThePathToCarcosa.Helpers

newtype ReturnToThePathToCarcosa = ReturnToThePathToCarcosa ThePathToCarcosa
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

instance IsCampaign ReturnToThePathToCarcosa where
  campaignTokens = campaignTokens @ThePathToCarcosa
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue ReturnToCurtainCall
    ReturnToCurtainCall -> continue ReturnToTheLastKing
    ReturnToTheLastKing -> continue ReturnToEchoesOfThePast
    InterludeStep 1 _ -> continue ReturnToEchoesOfThePast
    ReturnToEchoesOfThePast -> continue ReturnToTheUnspeakableOath
    ReturnToTheUnspeakableOath -> continue ReturnToAPhantomOfTruth
    InterludeStep 2 _ -> continue ReturnToAPhantomOfTruth
    ReturnToAPhantomOfTruth -> continue ReturnToThePallidMask
    ReturnToThePallidMask -> continue ReturnToBlackStarsRise
    ReturnToBlackStarsRise -> continue ReturnToDimCarcosa
    ReturnToDimCarcosa -> continue EpilogueStep
    other -> defaultNextStep other

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
