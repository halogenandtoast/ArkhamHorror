module Arkham.Treachery.Cards.AvariceCallsC (avariceCallsC) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AvariceCallsC = AvariceCallsC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avariceCallsC :: TreacheryCard AvariceCallsC
avariceCallsC = treachery AvariceCallsC Cards.avariceCallsC

instance RunMessage AvariceCallsC where
  runMessage msg t@(AvariceCallsC attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> AvariceCallsC <$> liftRunMessage msg attrs
