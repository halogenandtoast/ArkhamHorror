module Arkham.Campaign.Campaigns.ReturnToTheDunwichLegacy where

import Arkham.Prelude

import Arkham.Campaign.Campaigns.TheDunwichLegacy
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheDunwichLegacy.CampaignSteps
import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Campaigns.TheDunwichLegacy.Import
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
        Just NoResolution -> Just (UpgradeDeckStep UndimensionedAndUnseen)
        _ -> Just $ InterludeStep 2 Nothing
    InterludeStep 2 _ -> Just (UpgradeDeckStep UndimensionedAndUnseen)
    UndimensionedAndUnseen -> Just (UpgradeDeckStep WhereDoomAwaits)
    WhereDoomAwaits -> Just (UpgradeDeckStep LostInTimeAndSpace)
    LostInTimeAndSpace -> Just EpilogueStep
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
    CampaignStep PrologueStep -> do
      storyWithChooseOneM prologue do
        labeled
          "Professor Warren Rice was last seen working late at night in the humanities department of Miskatonic University. Let’s search for him there. Proceed with “Scenario I–A: Extracurricular Activity” if you wish to find Professor Warren Rice first."
          $ setNextCampaignStep ReturnToExtracurricularActivities
        labeled
          "Dr. Francis Morgan was last seen gambling at the Clover Club, an upscale speakeasy and gambling joint located downtown.  Let’s go talk to him.  Proceed with “Scenario I–B: The House Always Wins” if you wish to find Dr. Francis Morgan first."
          $ setNextCampaignStep ReturnToTheHouseAlwaysWins
      pure c
    NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
    _ -> ReturnToTheDunwichLegacy <$> liftRunMessage msg theDunwichLegacy'
