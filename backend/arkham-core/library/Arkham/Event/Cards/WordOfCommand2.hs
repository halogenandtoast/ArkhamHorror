module Arkham.Event.Cards.WordOfCommand2 (
  wordOfCommand2,
  WordOfCommand2 (..),
)
where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Classes
import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (Spell))

newtype WordOfCommand2 = WordOfCommand2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wordOfCommand2 :: EventCard WordOfCommand2
wordOfCommand2 = event WordOfCommand2 Cards.wordOfCommand2

instance RunMessage WordOfCommand2 where
  runMessage msg e@(WordOfCommand2 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      canSearch <- can.search.deck iid
      if canSearch
        then push $ search iid attrs iid [fromDeck] (CardWithTrait Spell) (DrawFoundUpTo iid 1)
        else push $ ShuffleDeck (InvestigatorDeck iid)
      pure e
    _ -> WordOfCommand2 <$> runMessage msg attrs
