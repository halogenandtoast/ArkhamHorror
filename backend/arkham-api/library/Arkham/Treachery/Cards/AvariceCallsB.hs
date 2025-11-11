module Arkham.Treachery.Cards.AvariceCallsB (avariceCallsB) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AvariceCallsB = AvariceCallsB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avariceCallsB :: TreacheryCard AvariceCallsB
avariceCallsB = treachery AvariceCallsB Cards.avariceCallsB

instance RunMessage AvariceCallsB where
  runMessage msg t@(AvariceCallsB attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> AvariceCallsB <$> liftRunMessage msg attrs
