module Arkham.Story.Cards.AShrineToTheGods (aShrineToTheGods) where

import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AShrineToTheGods = AShrineToTheGods StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aShrineToTheGods :: StoryCard AShrineToTheGods
aShrineToTheGods = story AShrineToTheGods Cards.aShrineToTheGods

instance RunMessage AShrineToTheGods where
  runMessage msg s@(AShrineToTheGods attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      tenebrousNightgaunts <- getSetAsideCardsMatching $ cardIs Enemies.tenebrousNightgaunt
      shuffleCardsIntoDeck Deck.EncounterDeck (take 1 tenebrousNightgaunts)
      pure s
    _ -> AShrineToTheGods <$> liftRunMessage msg attrs
