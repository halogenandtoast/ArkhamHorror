module Arkham.Campaign.Campaigns.ReturnToTheCircleUndone (returnToTheCircleUndone) where

import Arkham.Campaign.Campaigns.TheCircleUndone
import Arkham.Campaign.Import.Lifted
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.CampaignSteps
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Card

newtype ReturnToTheCircleUndone = ReturnToTheCircleUndone TheCircleUndone
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

returnToTheCircleUndone :: Difficulty -> ReturnToTheCircleUndone
returnToTheCircleUndone =
  campaignWith
    (ReturnToTheCircleUndone . TheCircleUndone)
    (CampaignId "54")
    "Return to The Circle Undone"
    $ logL
    .~ mkCampaignLog
      { campaignLogRecordedSets =
          singletonMap (toCampaignLogKey MissingPersons)
            $ map (recorded . cdCardCode) allPrologueInvestigators
      }

instance IsCampaign ReturnToTheCircleUndone where
  campaignTokens = campaignTokens @TheCircleUndone
  nextStep a = case (toAttrs a).normalizedStep of
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
    ReturnToBeforeTheBlackThrone -> Just EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other

instance RunMessage ReturnToTheCircleUndone where
  runMessage msg c@(ReturnToTheCircleUndone theCircleUndone') = runQueueT $ campaignI18n $ case msg of
    NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
    _ -> ReturnToTheCircleUndone <$> liftRunMessage msg theCircleUndone'
