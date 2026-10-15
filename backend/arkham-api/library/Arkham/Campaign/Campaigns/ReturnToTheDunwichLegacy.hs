module Arkham.Campaign.Campaigns.ReturnToTheDunwichLegacy (returnToTheDunwichLegacy) where

import Arkham.Campaign.Campaigns.TheDunwichLegacy
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheDunwichLegacy.CampaignSteps
import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Narrative (narrativeBuilder)
import Arkham.Message.Lifted.Choose
import Arkham.Narrative.TheDunwichLegacy (
  DunwichNarrative (..),
  addsFalseLeads,
  getDunwichNarrative,
 )
import Arkham.Resolution

newtype ReturnToTheDunwichLegacy = ReturnToTheDunwichLegacy TheDunwichLegacy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

instance IsCampaign ReturnToTheDunwichLegacy where
  campaignTokens = campaignTokens @TheDunwichLegacy
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> error $ "Unhandled campaign step: " <> show a
    ReturnToExtracurricularActivities ->
      if ReturnToTheHouseAlwaysWins `elem` (toAttrs a).completedSteps
        then continue $ InterludeStep 1 Nothing
        else continue ReturnToTheHouseAlwaysWins
    ReturnToTheHouseAlwaysWins ->
      if ReturnToExtracurricularActivities `elem` (toAttrs a).completedSteps
        then continue $ InterludeStep 1 Nothing
        else continue ReturnToExtracurricularActivities
    InterludeStep 1 _ -> continue ReturnToTheMiskatonicMuseum
    -- The box leaves the campaign's shape alone, so a narrative that adds an
    -- interlude here adds it to a "Return to" game too; its prose already does,
    -- since the scenarios are the same.
    ReturnToTheMiskatonicMuseum
      | addsFalseLeads (toAttrs a).log.options -> continue FalseLeads
    ReturnToTheMiskatonicMuseum -> continue ReturnToTheEssexCountyExpress
    FalseLeads -> continue ReturnToTheEssexCountyExpress
    ReturnToTheEssexCountyExpress -> continue ReturnToBloodOnTheAltar
    ReturnToBloodOnTheAltar ->
      case lookup "51032" (toAttrs a).resolutions of
        Just NoResolution -> continue ReturnToUndimensionedAndUnseen
        _ -> continue $ InterludeStep 2 Nothing
    InterludeStep 2 _ -> continue ReturnToUndimensionedAndUnseen
    ReturnToUndimensionedAndUnseen -> continue ReturnToWhereDoomAwaits
    ReturnToWhereDoomAwaits -> continue ReturnToLostInTimeAndSpace
    ReturnToLostInTimeAndSpace -> continue EpilogueStep
    other -> defaultNextStep other

returnToTheDunwichLegacy :: Difficulty -> ReturnToTheDunwichLegacy
returnToTheDunwichLegacy =
  campaign
    (ReturnToTheDunwichLegacy . TheDunwichLegacy)
    (CampaignId "51")
    "Return to the Dunwich Legacy"

instance RunMessage ReturnToTheDunwichLegacy where
  runMessage msg c@(ReturnToTheDunwichLegacy theDunwichLegacy') = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> scope "prologue" do
      -- The box reprints the campaign's prologue, so a narrative rewrites it
      -- here exactly as it does in the original.
      narrative <- getDunwichNarrative
      body <- narrativeBuilder narrative.prologue (setTitle "title" >> p "body")
      storyWithChooseOneM body do
        labeled "extracurricularActivity" $ setNextCampaignStep ReturnToExtracurricularActivities
        labeled "theHouseAlwaysWins" $ setNextCampaignStep ReturnToTheHouseAlwaysWins
      pure c
    NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
    _ -> ReturnToTheDunwichLegacy <$> liftRunMessage msg theDunwichLegacy'
