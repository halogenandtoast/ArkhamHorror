module Arkham.Treachery.Cards.AvariceCallsA (avariceCallsA) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AvariceCallsA = AvariceCallsA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avariceCallsA :: TreacheryCard AvariceCallsA
avariceCallsA = treachery AvariceCallsA Cards.avariceCallsA

instance RunMessage AvariceCallsA where
  runMessage msg t@(AvariceCallsA attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> AvariceCallsA <$> liftRunMessage msg attrs
