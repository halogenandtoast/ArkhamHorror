module Arkham.Act.Cards.TheUnvisitedIsle (
  TheUnvisitedIsle (..),
  theUnvisitedIsle,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Field
import Arkham.Location.Brazier
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Movement
import Data.List (cycle)
import Data.Map.Strict qualified as Map

newtype TheUnvisitedIsle = TheUnvisitedIsle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

theUnvisitedIsle :: ActCard TheUnvisitedIsle
theUnvisitedIsle = act (1, A) TheUnvisitedIsle Cards.theUnvisitedIsle (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage TheUnvisitedIsle where
  runMessage msg a@(TheUnvisitedIsle attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      investigators <- getInvestigators

      paired <- zip investigators <$> selectShuffled (SetAsideCardMatch "Unvisited Isle")
      sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      locationMessages <- flip concatMapM paired $ \(investigator, unvisitedIsle) -> do
        (lid, placement) <- placeLabeledLocation "unvisitedIsle" unvisitedIsle
        pure
          $ placement
          : PutLocationInFrontOf investigator lid
          : MoveTo
            ( uncancellableMove
                $ move attrs investigator lid
            )
          : [UpdateLocation lid (LocationBrazier ?=. Lit) | sidedWithTheCoven]

      -- We need to resolve all dealt cards in player order so we build a map first
      storyMap <-
        groupOnKey
          . zip (cycle investigators)
          <$> selectShuffled (UnderScenarioReferenceMatch StoryCard)

      -- then for each player in player order we get the corresponding story cards and resolve them
      let
        storyMessages = flip concatMap investigators $ \investigator ->
          let stories = Map.findWithDefault [] investigator storyMap
           in map (\s -> ReadStory investigator s ResolveIt Nothing) stories

      pushAll
        $ locationMessages
        <> storyMessages
        <> [advanceActDeck attrs]

      pure a
    _ -> TheUnvisitedIsle <$> runMessage msg attrs
