module Arkham.Story.Cards.TheDreamEaters.TheSearchForKadath.CylindersOfKadatheron (cylindersOfKadatheron) where

import Arkham.Deck qualified as Deck
import Arkham.Enemy.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Cards
import Arkham.Story.Import.Lifted

newtype CylindersOfKadatheron = CylindersOfKadatheron StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cylindersOfKadatheron :: StoryCard CylindersOfKadatheron
cylindersOfKadatheron = story CylindersOfKadatheron Cards.cylindersOfKadatheron

instance RunMessage CylindersOfKadatheron where
  runMessage msg s@(CylindersOfKadatheron attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      tenebrousNightgaunts <- getSetAsideCardsMatching $ cardIs Enemies.tenebrousNightgaunt
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      shuffleCardsIntoDeck Deck.EncounterDeck (take 1 tenebrousNightgaunts)
      pure s
    _ -> CylindersOfKadatheron <$> liftRunMessage msg attrs
