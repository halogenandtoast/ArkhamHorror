module Arkham.Treachery.Cards.Cornered (cornered) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Cornered = Cornered TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cornered :: TreacheryCard Cornered
cornered = treachery Cornered Cards.cornered

instance RunMessage Cornered where
  runMessage msg t@(Cornered attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Cornered <$> liftRunMessage msg attrs
