module Arkham.Campaign.Campaigns.ReturnToTheDunwichLegacy (
  ReturnToTheDunwichLegacy (..),
  returnToTheDunwichLegacy,
) where

import Arkham.Campaign.Campaigns.TheDunwichLegacy
import Arkham.Campaign.Runner
import Arkham.CampaignStep
import Arkham.Campaigns.TheDunwichLegacy.Import
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Prelude
import Arkham.Resolution

newtype ReturnToTheDunwichLegacy = ReturnToTheDunwichLegacy TheDunwichLegacy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

returnToTheDunwichLegacy :: Difficulty -> ReturnToTheDunwichLegacy
returnToTheDunwichLegacy difficulty =
  campaign
    (ReturnToTheDunwichLegacy . TheDunwichLegacy)
    (CampaignId "51")
    "Return to the Dunwich Legaxy"
    difficulty
    (chaosBagContents difficulty)

instance IsCampaign ReturnToTheDunwichLegacy where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> error $ "Unhandled campaign step: " <> show a
    ReturnToExtracurricularActivity ->
      if ReturnToTheHouseAlwaysWins `elem` campaignCompletedSteps (toAttrs a)
        then Just $ InterludeStep 1 Nothing
        else Just (UpgradeDeckStep ReturnToTheHouseAlwaysWins)
    ReturnToTheHouseAlwaysWins ->
      if ReturnToExtracurricularActivity `elem` campaignCompletedSteps (toAttrs a)
        then Just $ InterludeStep 1 Nothing
        else Just (UpgradeDeckStep ReturnToExtracurricularActivity)
    InterludeStep 1 _ -> Just (UpgradeDeckStep ReturnToTheMiskatonicMuseum)
    ReturnToTheMiskatonicMuseum -> Just (UpgradeDeckStep ReturnToTheEssexCountyExpress)
    ReturnToTheEssexCountyExpress -> Just (UpgradeDeckStep ReturnToBloodOnTheAltar)
    ReturnToBloodOnTheAltar ->
      case lookup "51032" (campaignResolutions $ toAttrs a) of
        Just NoResolution -> Just (UpgradeDeckStep ReturnToUndimensionedAndUnseen)
        _ -> Just $ InterludeStep 2 Nothing
    InterludeStep 2 _ -> Just (UpgradeDeckStep ReturnToUndimensionedAndUnseen)
    ReturnToUndimensionedAndUnseen -> Just (UpgradeDeckStep ReturnToWhereDoomAwaits)
    ReturnToWhereDoomAwaits -> Just (UpgradeDeckStep ReturnToLostInTimeAndSpace)
    ReturnToLostInTimeAndSpace -> Just EpilogueStep
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

instance RunMessage ReturnToTheDunwichLegacy where
  runMessage msg c@(ReturnToTheDunwichLegacy base) = case msg of
    CampaignStep PrologueStep -> do
      players <- allPlayers
      lead <- getLeadPlayer
      pushAll
        [ storyWithChooseOne
            lead
            players
            prologue
            [ Label
                "Professor Warren Rice was last seen working late at night in the humanities department of Miskatonic University. Let’s search for him there. Proceed with “Scenario I–A: Extracurricular Activity” if you wish to find Professor Warren Rice first."
                [NextCampaignStep (Just ReturnToExtracurricularActivity)]
            , Label
                "Dr. Francis Morgan was last seen gambling at the Clover Club, an upscale speakeasy and gambling joint located downtown.  Let’s go talk to him.  Proceed with “Scenario I–B: The House Always Wins” if you wish to find Dr. Francis Morgan first."
                [NextCampaignStep (Just ReturnToTheHouseAlwaysWins)]
            ]
        ]
      pure c
    _ -> ReturnToTheDunwichLegacy <$> runMessage msg base
