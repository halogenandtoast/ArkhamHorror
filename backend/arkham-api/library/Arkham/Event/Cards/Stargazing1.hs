module Arkham.Event.Cards.Stargazing1 (
  stargazing1,
  Stargazing1 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator

newtype Stargazing1 = Stargazing1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stargazing1 :: EventCard Stargazing1
stargazing1 = event Stargazing1 Cards.stargazing1

instance RunMessage Stargazing1 where
  runMessage msg e@(Stargazing1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      mTheStarsAreRight <- listToMaybe <$> searchBonded iid Cards.theStarsAreRight
      case mTheStarsAreRight of
        Nothing -> error "should not have been playable"
        Just theStarsAreRight -> do
          push $ ShuffleCardsIntoTopOfDeck Deck.EncounterDeck 10 [theStarsAreRight]
      pure e
    _ -> Stargazing1 <$> runMessage msg attrs
