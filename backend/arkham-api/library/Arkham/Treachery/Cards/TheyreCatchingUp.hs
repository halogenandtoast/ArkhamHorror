module Arkham.Treachery.Cards.TheyreCatchingUp
  ( theyreCatchingUp
  , TheyreCatchingUp(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheyreCatchingUp = TheyreCatchingUp TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyreCatchingUp :: TreacheryCard TheyreCatchingUp
theyreCatchingUp = treachery TheyreCatchingUp Cards.theyreCatchingUp

instance RunMessage TheyreCatchingUp where
  runMessage msg t@(TheyreCatchingUp attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TheyreCatchingUp <$> liftRunMessage msg attrs
