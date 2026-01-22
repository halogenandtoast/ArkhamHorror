module Arkham.Treachery.Cards.Calcification (calcification) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Calcification = Calcification TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

calcification :: TreacheryCard Calcification
calcification = treachery Calcification Cards.calcification

instance RunMessage Calcification where
  runMessage msg t@(Calcification attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Calcification <$> liftRunMessage msg attrs
