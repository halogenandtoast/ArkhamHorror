module Arkham.Story.Cards.AdviceOfTheKing (adviceOfTheKing) where

import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AdviceOfTheKing = AdviceOfTheKing StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adviceOfTheKing :: StoryCard AdviceOfTheKing
adviceOfTheKing = story AdviceOfTheKing Cards.adviceOfTheKing

instance RunMessage AdviceOfTheKing where
  runMessage msg s@(AdviceOfTheKing attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      remember BeseechedTheKing
      tenebrousNightgaunts <- getSetAsideCardsMatching $ cardIs Enemies.tenebrousNightgaunt
      shuffleCardsIntoDeck Deck.EncounterDeck (take 1 tenebrousNightgaunts)
      pure s
    _ -> AdviceOfTheKing <$> liftRunMessage msg attrs
