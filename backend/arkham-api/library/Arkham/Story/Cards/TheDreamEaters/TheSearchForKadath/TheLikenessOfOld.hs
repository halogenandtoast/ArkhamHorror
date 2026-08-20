module Arkham.Story.Cards.TheDreamEaters.TheSearchForKadath.TheLikenessOfOld (theLikenessOfOld) where

import Arkham.Deck qualified as Deck
import Arkham.Enemy.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheLikenessOfOld = TheLikenessOfOld StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLikenessOfOld :: StoryCard TheLikenessOfOld
theLikenessOfOld = story TheLikenessOfOld Cards.theLikenessOfOld

instance RunMessage TheLikenessOfOld where
  runMessage msg s@(TheLikenessOfOld attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      tenebrousNightgaunts <- getSetAsideCardsMatching $ cardIs Enemies.tenebrousNightgaunt
      shuffleCardsIntoDeck Deck.EncounterDeck (take 1 tenebrousNightgaunts)
      pure s
    _ -> TheLikenessOfOld <$> liftRunMessage msg attrs
