module Arkham.Treachery.Cards.MiasmaticTorment (miasmaticTorment) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MiasmaticTorment = MiasmaticTorment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miasmaticTorment :: TreacheryCard MiasmaticTorment
miasmaticTorment = treachery MiasmaticTorment Cards.miasmaticTorment

instance RunMessage MiasmaticTorment where
  runMessage msg t@(MiasmaticTorment attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> MiasmaticTorment <$> liftRunMessage msg attrs
