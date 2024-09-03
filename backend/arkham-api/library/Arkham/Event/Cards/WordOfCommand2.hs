module Arkham.Event.Cards.WordOfCommand2 (wordOfCommand2, WordOfCommand2 (..)) where

import Arkham.Capability
import Arkham.Classes
import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Prelude

newtype WordOfCommand2 = WordOfCommand2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wordOfCommand2 :: EventCard WordOfCommand2
wordOfCommand2 = event WordOfCommand2 Cards.wordOfCommand2

instance RunMessage WordOfCommand2 where
  runMessage msg e@(WordOfCommand2 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      canSearch <- can.search.deck iid
      push
        $ if canSearch
          then search iid attrs iid [fromDeck] #spell (DrawFoundUpTo iid 1)
          else ShuffleDeck (InvestigatorDeck iid)
      pure e
    _ -> WordOfCommand2 <$> runMessage msg attrs
