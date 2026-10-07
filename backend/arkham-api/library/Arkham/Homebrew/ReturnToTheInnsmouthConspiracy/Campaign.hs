{- | The (Unofficial) Return to The Innsmouth Conspiracy is not a campaign of its
own: it plays the official The Innsmouth Conspiracy with replacement encounter
sets and new versions of some of its cards. So it is a newtype over that
campaign and delegates every message to it -- the campaign log, the flashbacks,
all four interludes and both endings are the official ones. Only 'nextStep'
differs, pointing at this box's scenario reference cards instead.
-}
module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Campaign (returnToTheInnsmouthConspiracy) where

import Arkham.Campaign.Campaigns.TheInnsmouthConspiracy
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CampaignSteps
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers
import Arkham.Message.Lifted.Log (recordSetInsert)

newtype ReturnToTheInnsmouthConspiracy = ReturnToTheInnsmouthConspiracy TheInnsmouthConspiracy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

returnToTheInnsmouthConspiracy :: Difficulty -> ReturnToTheInnsmouthConspiracy
returnToTheInnsmouthConspiracy =
  campaign
    (ReturnToTheInnsmouthConspiracy . TheInnsmouthConspiracy)
    (CampaignId ":return-to-the-innsmouth-conspiracy")
    "Return to The Innsmouth Conspiracy"

instance IsCampaign ReturnToTheInnsmouthConspiracy where
  campaignTokens = campaignTokens @TheInnsmouthConspiracy
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue ReturnToThePitOfDespair
    ReturnToThePitOfDespair -> continue $ InterludeStep 1 Nothing
    InterludeStep 1 _ -> continueNoUpgrade ReturnToTheVanishingOfElinaHarper
    ReturnToTheVanishingOfElinaHarper -> continue $ InterludeStep 2 Nothing
    InterludeStep 2 _ -> continue ReturnToInTooDeep
    ReturnToInTooDeep -> continueNoUpgrade ReturnToDevilReef
    -- Devil Reef ends with `endOfScenarioThen (InterludeStep 3 ...)` because the
    -- interlude needs to know which keys were claimed, so it has no case here.
    InterludeStep 3 _ -> continue ReturnToHorrorInHighGear
    ReturnToHorrorInHighGear -> continue ReturnToALightInTheFog
    ReturnToALightInTheFog -> continueNoUpgrade ReturnToTheLairOfDagon
    ReturnToTheLairOfDagon -> continue $ InterludeStep 4 Nothing
    InterludeStep 4 _ -> continue ReturnToIntoTheMaelstrom
    ReturnToIntoTheMaelstrom -> continue EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other

instance RunMessage ReturnToTheInnsmouthConspiracy where
  runMessage msg c@(ReturnToTheInnsmouthConspiracy innsmouthConspiracy') = runQueueT $ campaignI18n $ case msg of
    NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
    -- "Before reading the Epilogue, every Deep One investigator has to read this:
    -- FLASHBACK XVI". Read before delegating, so it lands ahead of the official epilogue.
    CampaignStep EpilogueStep -> do
      deepOnes <- deepOneInvestigatorsInCampaign
      unless (null deepOnes) do
        story $ i18nWithTitle "flashbackXVI"
        recordSetInsert MemoriesRecovered [toJSON youRememberWhereYouHaveToGo]
      ReturnToTheInnsmouthConspiracy <$> liftRunMessage msg innsmouthConspiracy'
    _ -> ReturnToTheInnsmouthConspiracy <$> liftRunMessage msg innsmouthConspiracy'
