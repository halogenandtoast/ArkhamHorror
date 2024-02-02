module Arkham.Treachery.Cards.OutOfBodyExperience (
  outOfBodyExperience,
  OutOfBodyExperience (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype OutOfBodyExperience = OutOfBodyExperience TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

outOfBodyExperience :: TreacheryCard OutOfBodyExperience
outOfBodyExperience = treachery OutOfBodyExperience Cards.outOfBodyExperience

instance RunMessage OutOfBodyExperience where
  runMessage msg t@(OutOfBodyExperience attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      result <- withoutModifier (InvestigatorTarget iid) CannotManipulateDeck
      when result $ do
        cards <- field InvestigatorHand iid
        drawing <- drawCards iid attrs (length cards)
        pushAll
          [ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) cards
          , drawing
          , RemoveTreachery (toId attrs)
          , ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [toCard attrs]
          ]
      pure t
    _ -> OutOfBodyExperience <$> runMessage msg attrs
