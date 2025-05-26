module Arkham.Campaign.Campaigns.ReturnToTheForgottenAge (returnToTheForgottenAge) where

import Arkham.Campaign.Campaigns.TheForgottenAge
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.CampaignSteps
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Import

newtype ReturnToTheForgottenAge = ReturnToTheForgottenAge TheForgottenAge
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

instance IsCampaign ReturnToTheForgottenAge where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just ReturnToTheUntamedWilds
    ReturnToTheUntamedWilds -> Just (InterludeStep 1 Nothing)
    InterludeStep 1 _ -> Just (UpgradeDeckStep ReturnToTheDoomOfEztli)
    ReturnToTheDoomOfEztli -> Just (UpgradeDeckStep $ InterludeStep 2 Nothing)
    InterludeStep 2 _ -> Just ReturnToThreadsOfFate
    ReturnToThreadsOfFate -> Just ResupplyPoint
    ResupplyPoint -> Just (UpgradeDeckStep ReturnToTheBoundaryBeyond)
    ReturnToTheBoundaryBeyond -> Just (UpgradeDeckStep $ InterludeStep 3 Nothing)
    InterludeStep 3 _ -> Just ReturnToHeartOfTheElders
    ReturnToHeartOfTheElders -> Just (UpgradeDeckStep ReturnToTheCityOfArchives)
    ReturnToTheCityOfArchives -> Just (UpgradeDeckStep $ InterludeStep 4 Nothing)
    InterludeStep 4 _ -> Just ReturnToTheDepthsOfYoth
    ReturnToTheDepthsOfYoth -> Just (UpgradeDeckStep $ InterludeStep 5 Nothing)
    InterludeStep 5 _ -> Just ReturnToShatteredAeons
    ReturnToShatteredAeons -> Nothing
    EpilogueStep -> Just (UpgradeDeckStep ReturnToTurnBackTime)
    ReturnToTurnBackTime -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

returnToTheForgottenAge :: Difficulty -> ReturnToTheForgottenAge
returnToTheForgottenAge difficulty =
  campaign
    (ReturnToTheForgottenAge . TheForgottenAge)
    (CampaignId "53")
    "Return to the Forgotten Age"
    difficulty
    (chaosBagContents difficulty)

instance RunMessage ReturnToTheForgottenAge where
  runMessage msg c@(ReturnToTheForgottenAge theForgottenAge') = runQueueT $ campaignI18n $ case msg of
    NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
    _ -> ReturnToTheForgottenAge <$> liftRunMessage msg theForgottenAge'
