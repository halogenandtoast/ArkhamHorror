module Arkham.Event.Events.Stargazing1 (stargazing1) where

import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator

newtype Stargazing1 = Stargazing1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stargazing1 :: EventCard Stargazing1
stargazing1 = event Stargazing1 Cards.stargazing1

instance RunMessage Stargazing1 where
  runMessage msg e@(Stargazing1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      mTheStarsAreRight <- listToMaybe <$> searchBonded iid Cards.theStarsAreRight
      case mTheStarsAreRight of
        Nothing -> error "should not have been playable"
        Just theStarsAreRight -> shuffleCardsIntoTopOfDeck Deck.EncounterDeck 10 [theStarsAreRight]
      pure e
    _ -> Stargazing1 <$> liftRunMessage msg attrs
