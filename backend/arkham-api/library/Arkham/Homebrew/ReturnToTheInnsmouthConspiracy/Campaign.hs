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
import Arkham.CampaignLogKey (recorded)
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Helpers.Log (getRecordSet)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Achievements (
  runReturnToTheInnsmouthConspiracyAchievements,
 )
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
  runMessage msg c@(ReturnToTheInnsmouthConspiracy innsmouthConspiracy') =
    runQueueT
      $ campaignI18n
      $ lift (runReturnToTheInnsmouthConspiracyAchievements msg)
      *> case msg of
        NextCampaignStep _ -> lift $ defaultCampaignRunner msg c
        -- "Before reading the Epilogue, every Deep One investigator has to read this:
        -- FLASHBACK XVI". Read before delegating, so it lands ahead of the official epilogue.
        CampaignStep EpilogueStep -> do
          deepOnes <- deepOneInvestigatorsInCampaign
          if null deepOnes
            then ReturnToTheInnsmouthConspiracy <$> liftRunMessage msg innsmouthConspiracy'
            else do
              story $ i18nWithTitle "flashbackXVI"
              recordSetInsert MemoriesRecovered [toJSON youRememberWhereYouHaveToGo]
              {- "During the Epilogue, this memory can stand in for another one when determining
              whether you get to read Flashback XV (you still need 14 or more unlocked
              Flashbacks, including this one)." The Campaign Guide asks for every memory on its
              list, which no substitute can satisfy, so the box counts them instead and lets
              Flashback XVI make up one shortfall. Everything read here is the Campaign Guide's
              own text. -}
              official "epilogue" do
                readEpilogue1
                memories <- getRecordSet MemoriesRecovered
                let recovered = count ((`elem` memories) . recorded . fst) flashback15Memories
                if recovered + 1 >= length flashback15Memories
                  then do
                    recordTheHorribleTruth
                    story $ i18nWithTitle "flashback15"
                  else story $ i18nWithTitle "epilogue2"
              gameOver
              pure c
        _ -> ReturnToTheInnsmouthConspiracy <$> liftRunMessage msg innsmouthConspiracy'
