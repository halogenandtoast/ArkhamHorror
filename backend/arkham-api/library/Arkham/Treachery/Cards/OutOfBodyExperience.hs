module Arkham.Treachery.Cards.OutOfBodyExperience (outOfBodyExperience) where

import Arkham.Capability
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OutOfBodyExperience = OutOfBodyExperience TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfBodyExperience :: TreacheryCard OutOfBodyExperience
outOfBodyExperience = treachery OutOfBodyExperience Cards.outOfBodyExperience

instance RunMessage OutOfBodyExperience where
  runMessage msg t@(OutOfBodyExperience attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      whenM (can.manipulate.deck iid) do
        cards <- field InvestigatorHand iid
        shuffleCardsIntoDeck iid cards
        drawCards iid attrs (length cards)
        removeTreachery attrs
        shuffleCardsIntoDeck iid [toCard attrs]
      pure t
    _ -> OutOfBodyExperience <$> liftRunMessage msg attrs
