module Arkham.Story.Cards.TheLikenessOfOld (TheLikenessOfOld (..), theLikenessOfOld) where

import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheLikenessOfOld = TheLikenessOfOld StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theLikenessOfOld :: StoryCard TheLikenessOfOld
theLikenessOfOld = story TheLikenessOfOld Cards.theLikenessOfOld

instance RunMessage TheLikenessOfOld where
  runMessage msg s@(TheLikenessOfOld attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      tenebrousNightgaunts <- getSetAsideCardsMatching $ cardIs Enemies.tenebrousNightgaunt
      pushAll
        $ ScenarioCountIncrementBy SignOfTheGods 1
        : [ShuffleCardsIntoDeck Deck.EncounterDeck [card] | card <- take 1 tenebrousNightgaunts]
      pure s
    _ -> TheLikenessOfOld <$> runMessage msg attrs
