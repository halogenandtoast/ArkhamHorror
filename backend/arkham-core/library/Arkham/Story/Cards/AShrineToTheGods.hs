module Arkham.Story.Cards.AShrineToTheGods (AShrineToTheGods (..), aShrineToTheGods) where

import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype AShrineToTheGods = AShrineToTheGods StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

aShrineToTheGods :: StoryCard AShrineToTheGods
aShrineToTheGods = story AShrineToTheGods Cards.aShrineToTheGods

instance RunMessage AShrineToTheGods where
  runMessage msg s@(AShrineToTheGods attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      tenebrousNightgaunts <- getSetAsideCardsMatching $ cardIs Enemies.tenebrousNightgaunt
      pushAll
        $ ScenarioCountIncrementBy SignOfTheGods 1
        : [ShuffleCardsIntoDeck Deck.EncounterDeck [card] | card <- take 1 tenebrousNightgaunts]
      pure s
    _ -> AShrineToTheGods <$> runMessage msg attrs
