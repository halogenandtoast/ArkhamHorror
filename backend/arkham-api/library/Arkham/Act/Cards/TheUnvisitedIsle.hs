module Arkham.Act.Cards.TheUnvisitedIsle (TheUnvisitedIsle (..), theUnvisitedIsle) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Field
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Location.Brazier
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (StoryMode (..))
import Arkham.Movement
import Data.List (cycle)
import Data.Map.Strict qualified as Map

newtype TheUnvisitedIsle = TheUnvisitedIsle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theUnvisitedIsle :: ActCard TheUnvisitedIsle
theUnvisitedIsle = act (1, A) TheUnvisitedIsle Cards.theUnvisitedIsle (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage TheUnvisitedIsle where
  runMessage msg a@(TheUnvisitedIsle attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      eachInvestigator \iid -> push $ ForInvestigator iid msg
      investigators <- getInvestigators

      -- We need to resolve all dealt cards in player order so we build a map first
      storyMap <-
        groupOnKey
          . zip (cycle investigators)
          <$> selectShuffled (UnderScenarioReferenceMatch StoryCard)

      -- then for each player in player order we get the corresponding story cards and resolve them
      for_ investigators \investigator -> do
        let stories = Map.findWithDefault [] investigator storyMap
        pushAll $ map (\s -> ReadStory investigator s ResolveIt Nothing) stories

      advanceActDeck attrs
      pure a
    ForInvestigator iid (AdvanceAct (isSide B attrs -> True) _ _) -> do
      paired <- take 1 <$> selectShuffled (SetAsideCardMatch "Unvisited Isle")
      sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      for_ paired \unvisitedIsle -> do
        lid <- placeLabeledLocation "unvisitedIsle" unvisitedIsle
        pushAll
          $ PutLocationInFrontOf iid lid
          : Move (uncancellableMove $ move attrs iid lid)
          : [UpdateLocation lid (LocationBrazier ?=. Lit) | sidedWithTheCoven]
      pure a
    _ -> TheUnvisitedIsle <$> liftRunMessage msg attrs
