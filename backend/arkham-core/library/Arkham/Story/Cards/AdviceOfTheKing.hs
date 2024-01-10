module Arkham.Story.Cards.AdviceOfTheKing (AdviceOfTheKing (..), adviceOfTheKing) where

import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype AdviceOfTheKing = AdviceOfTheKing StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adviceOfTheKing :: StoryCard AdviceOfTheKing
adviceOfTheKing = story AdviceOfTheKing Cards.adviceOfTheKing

instance RunMessage AdviceOfTheKing where
  runMessage msg s@(AdviceOfTheKing attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      tenebrousNightgaunts <- getSetAsideCardsMatching $ cardIs Enemies.tenebrousNightgaunt
      pushAll
        $ Remember BeseechedTheKing
        : [ShuffleCardsIntoDeck Deck.EncounterDeck [card] | card <- take 1 tenebrousNightgaunts]
      pure s
    _ -> AdviceOfTheKing <$> runMessage msg attrs
