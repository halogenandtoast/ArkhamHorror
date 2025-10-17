module Arkham.Treachery.Cards.CallingCard (callingCard) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CallingCard = CallingCard TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingCard :: TreacheryCard CallingCard
callingCard = treachery CallingCard Cards.callingCard

instance RunMessage CallingCard where
  runMessage msg t@(CallingCard attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> CallingCard <$> liftRunMessage msg attrs
