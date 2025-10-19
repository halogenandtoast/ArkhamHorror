module Arkham.Treachery.Cards.Accosted (accosted) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Accosted = Accosted TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accosted :: TreacheryCard Accosted
accosted = treachery Accosted Cards.accosted

instance RunMessage Accosted where
  runMessage msg t@(Accosted attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Accosted <$> liftRunMessage msg attrs
