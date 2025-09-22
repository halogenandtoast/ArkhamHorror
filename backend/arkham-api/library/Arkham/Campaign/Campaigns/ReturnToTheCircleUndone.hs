module Arkham.Campaign.Campaigns.ReturnToTheCircleUndone (returnToTheCircleUndone) where

import Arkham.Campaign.Campaigns.TheCircleUndone
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.CampaignSteps
import Arkham.Campaigns.TheCircleUndone.Helpers

newtype ReturnToTheCircleUndone = ReturnToTheCircleUndone TheCircleUndone
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

returnToTheCircleUndone :: Difficulty -> ReturnToTheCircleUndone
returnToTheCircleUndone =
  campaign
    (ReturnToTheCircleUndone . TheCircleUndone)
    (CampaignId "54")
    "Return to The Circle Undone"

instance IsCampaign ReturnToTheCircleUndone where
  campaignTokens = campaignTokens @TheCircleUndone
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just ReturnToDisappearanceAtTheTwilightEstate
    ReturnToDisappearanceAtTheTwilightEstate -> Just ReturnToTheWitchingHour
    ReturnToTheWitchingHour -> Just (UpgradeDeckStep ReturnToAtDeathsDoorstep)
    ReturnToAtDeathsDoorstep -> Just (UpgradeDeckStep ReturnToTheSecretName)
    InterludeStep 2 _ -> Just (UpgradeDeckStep ReturnToTheSecretName)
    ReturnToTheSecretName -> Just (UpgradeDeckStep ReturnToTheWagesOfSin)
    ReturnToTheWagesOfSin -> Just (UpgradeDeckStep ReturnToForTheGreaterGood)
    ReturnToForTheGreaterGood -> Just (UpgradeDeckStep ReturnToUnionAndDisillusion)
    InterludeStep 3 _ -> Just (UpgradeDeckStep ReturnToUnionAndDisillusion)
    ReturnToUnionAndDisillusion -> Just (UpgradeDeckStep ReturnToInTheClutchesOfChaos)
    ReturnToInTheClutchesOfChaos -> Just (UpgradeDeckStep $ InterludeStep 4 Nothing)
    InterludeStep 4 _ -> Just (UpgradeDeckStep ReturnToBeforeTheBlackThrone)
    ReturnToBeforeTheBlackThrone -> Nothing
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

instance RunMessage ReturnToTheCircleUndone where
  runMessage msg c@(ReturnToTheCircleUndone theCircleUndone') = runQueueT $ campaignI18n $ case msg of
    NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
    _ -> ReturnToTheCircleUndone <$> liftRunMessage msg theCircleUndone'
