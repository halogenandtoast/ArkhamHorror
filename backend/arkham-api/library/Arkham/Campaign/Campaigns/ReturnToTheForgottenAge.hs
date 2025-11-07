module Arkham.Campaign.Campaigns.ReturnToTheForgottenAge (returnToTheForgottenAge) where

import Arkham.Campaign.Campaigns.TheForgottenAge
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.CampaignSteps
import Arkham.Campaigns.TheForgottenAge.Helpers

newtype ReturnToTheForgottenAge = ReturnToTheForgottenAge TheForgottenAge
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

instance IsCampaign ReturnToTheForgottenAge where
  campaignTokens = campaignTokens @TheForgottenAge
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue ReturnToTheUntamedWilds
    ReturnToTheUntamedWilds -> continue (InterludeStep 1 Nothing)
    InterludeStep 1 _ -> continue ReturnToTheDoomOfEztli
    ReturnToTheDoomOfEztli -> continue $ InterludeStep 2 Nothing
    InterludeStep 2 _ -> continue ReturnToThreadsOfFate
    ReturnToThreadsOfFate -> continue ResupplyPoint
    ResupplyPoint -> continue ReturnToTheBoundaryBeyond
    ReturnToTheBoundaryBeyond -> continue $ InterludeStep 3 Nothing
    InterludeStep 3 _ -> continue ReturnToHeartOfTheEldersPart1
    ReturnToHeartOfTheEldersPart1 -> continueNoUpgrade ReturnToHeartOfTheEldersPart2
    ReturnToHeartOfTheEldersPart2 -> continue ReturnToTheCityOfArchives
    ReturnToTheCityOfArchives -> continue $ InterludeStep 4 Nothing
    InterludeStep 4 _ -> continue ReturnToTheDepthsOfYoth
    ReturnToTheDepthsOfYoth -> continue $ InterludeStep 5 Nothing
    InterludeStep 5 _ -> continue ReturnToShatteredAeons
    ReturnToShatteredAeons -> Nothing
    EpilogueStep -> continue ReturnToTurnBackTime
    ReturnToTurnBackTime -> Nothing
    other -> defaultNextStep other

returnToTheForgottenAge :: Difficulty -> ReturnToTheForgottenAge
returnToTheForgottenAge =
  campaign
    (ReturnToTheForgottenAge . TheForgottenAge)
    (CampaignId "53")
    "Return to the Forgotten Age"

instance RunMessage ReturnToTheForgottenAge where
  runMessage msg c@(ReturnToTheForgottenAge theForgottenAge') = runQueueT $ campaignI18n $ case msg of
    NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
    _ -> ReturnToTheForgottenAge <$> liftRunMessage msg theForgottenAge'
