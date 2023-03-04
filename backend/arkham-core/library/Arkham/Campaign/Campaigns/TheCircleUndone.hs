module Arkham.Campaign.Campaigns.TheCircleUndone
  ( TheCircleUndone(..)
  , theCircleUndone
  ) where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.Campaigns.TheCircleUndone.Import
import Arkham.CampaignStep
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Message

newtype Metadata = Metadata
  { prologueInvestigators :: HashMap InvestigatorId InvestigatorId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheCircleUndone = TheCircleUndone (CampaignAttrs `With` Metadata)
  deriving anyclass IsCampaign
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theCircleUndone :: Difficulty -> TheCircleUndone
theCircleUndone difficulty = campaign
  (TheCircleUndone . (`with` Metadata mempty))
  (CampaignId "05")
  "The Circle Undone"
  difficulty
  (chaosBagContents difficulty)

allPrologueInvestigators :: [InvestigatorId]
allPrologueInvestigators = ["05046", "05047", "05048", "05049"]

instance RunMessage TheCircleUndone where
  runMessage msg c@(TheCircleUndone (attrs `With` metadata)) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- allInvestigatorIds
      pushAll $ story investigatorIds prologue
        : [ CampaignStep (Just (InvestigatorCampaignStep iid PrologueStep))
           | iid <- investigatorIds
           ]
        <> [CampaignStep (Just $ PrologueStepPart 2), story investigatorIds intro, NextCampaignStep Nothing]
      pure c
    CampaignStep (Just (InvestigatorCampaignStep iid PrologueStep)) -> do
      let availablePrologueInvestigators = filter (`notElem` toList (prologueInvestigators metadata)) allPrologueInvestigators
      push $ questionLabel "Choose one of the following neutral investigators to control for the duration of this prologue" iid $ ChooseOne [CardLabel (unInvestigatorId pId) [BecomePrologueInvestigator iid pId] | pId <- availablePrologueInvestigators]
      pure c
    BecomePrologueInvestigator iid pId -> do
      pure . TheCircleUndone $ attrs `With` metadata { prologueInvestigators = insertMap iid pId (prologueInvestigators metadata) }
    CampaignStep (Just (PrologueStepPart 2)) -> do
      let
        readingFor = \case
          "05046" -> gavriellaIntro
          "05047" -> jeromeIntro
          "05048" -> pennyIntro
          "05049" -> valentinoIntro
          _ -> error "Invalid prologue investigator"
        readings = map readingFor $ toList (prologueInvestigators metadata)
      investigatorIds <- getInvestigatorIds
      pushAll $ map (story investigatorIds) readings
      pure c
    NextCampaignStep _ -> do
      let step = nextStep attrs
      push $ CampaignStep step
      pure
        . TheCircleUndone
        . (`with` metadata)
        $ attrs
        & (stepL .~ step)
        & (completedStepsL %~ completeStep (campaignStep attrs))
    PreScenarioSetup -> do
      case mapToList (prologueInvestigators metadata) of
        [] -> pure ()
        xs -> pushAll $ map (uncurry BecomePrologueInvestigator) xs
      pure c
    ResetGame -> do
      case mapToList (prologueInvestigators metadata) of
        [] -> TheCircleUndone . (`with` metadata) <$> runMessage msg attrs
        xs -> do
          for_ xs $ \(iid, _) -> push $ LoadDeck iid $ Deck []
          pure c
    _ -> TheCircleUndone . (`with` metadata) <$> runMessage msg attrs
