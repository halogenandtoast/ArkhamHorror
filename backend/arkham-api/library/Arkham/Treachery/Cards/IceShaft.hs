module Arkham.Treachery.Cards.IceShaft (iceShaft, IceShaft (..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype IceShaft = IceShaft TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iceShaft :: TreacheryCard IceShaft
iceShaft = treachery IceShaft Cards.iceShaft

instance RunMessage IceShaft where
  runMessage msg t@(IceShaft attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> IceShaft <$> liftRunMessage msg attrs
