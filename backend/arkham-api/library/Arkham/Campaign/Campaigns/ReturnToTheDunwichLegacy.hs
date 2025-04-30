module Arkham.Campaign.Campaigns.ReturnToTheDunwichLegacy (returnToTheDunwichLegacy) where

import Arkham.Campaign.Campaigns.TheDunwichLegacy
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheDunwichLegacy.CampaignSteps
import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Campaigns.TheDunwichLegacy.Import
import Arkham.Helpers.FlavorText
import Arkham.Message.Lifted.Choose
import Arkham.Resolution

newtype ReturnToTheDunwichLegacy = ReturnToTheDunwichLegacy TheDunwichLegacy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

instance IsCampaign ReturnToTheDunwichLegacy where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> error $ "Unhandled campaign step: " <> show a
    ReturnToExtracurricularActivities ->
      if ReturnToTheHouseAlwaysWins `elem` (toAttrs a).completedSteps
        then Just $ InterludeStep 1 Nothing
        else Just (UpgradeDeckStep ReturnToTheHouseAlwaysWins)
    ReturnToTheHouseAlwaysWins ->
      if ReturnToExtracurricularActivities `elem` (toAttrs a).completedSteps
        then Just $ InterludeStep 1 Nothing
        else Just (UpgradeDeckStep ReturnToExtracurricularActivities)
    InterludeStep 1 _ -> Just (UpgradeDeckStep ReturnToTheMiskatonicMuseum)
    ReturnToTheMiskatonicMuseum -> Just (UpgradeDeckStep ReturnToTheEssexCountyExpress)
    ReturnToTheEssexCountyExpress -> Just (UpgradeDeckStep ReturnToBloodOnTheAltar)
    ReturnToBloodOnTheAltar ->
      case lookup "51032" (toAttrs a).resolutions of
        Just NoResolution -> Just (UpgradeDeckStep ReturnToUndimensionedAndUnseen)
        _ -> Just $ InterludeStep 2 Nothing
    InterludeStep 2 _ -> Just (UpgradeDeckStep ReturnToUndimensionedAndUnseen)
    ReturnToUndimensionedAndUnseen -> Just (UpgradeDeckStep ReturnToWhereDoomAwaits)
    ReturnToWhereDoomAwaits -> Just (UpgradeDeckStep ReturnToLostInTimeAndSpace)
    ReturnToLostInTimeAndSpace -> Just EpilogueStep
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

returnToTheDunwichLegacy :: Difficulty -> ReturnToTheDunwichLegacy
returnToTheDunwichLegacy difficulty =
  campaign
    (ReturnToTheDunwichLegacy . TheDunwichLegacy)
    (CampaignId "51")
    "Return to the Dunwich Legacy"
    difficulty
    (chaosBagContents difficulty)

instance RunMessage ReturnToTheDunwichLegacy where
  runMessage msg c@(ReturnToTheDunwichLegacy theDunwichLegacy') = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> scope "prologue" do
      storyWithChooseOneM' (setTitle "title" >> p "body") do
        labeled' "extracurricularActivity" $ setNextCampaignStep ReturnToExtracurricularActivities
        labeled' "theHouseAlwaysWins" $ setNextCampaignStep ReturnToTheHouseAlwaysWins
      pure c
    NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
    _ -> ReturnToTheDunwichLegacy <$> liftRunMessage msg theDunwichLegacy'
