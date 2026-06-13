module Arkham.Act.Cards.ThePhantomShop (thePhantomShop) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ThePhantomShop = ThePhantomShop ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePhantomShop :: ActCard ThePhantomShop
thePhantomShop = act (1, A) ThePhantomShop Cards.thePhantomShop Nothing

-- TODO: abilities
instance HasAbilities ThePhantomShop where
  getAbilities _ = []

instance RunMessage ThePhantomShop where
  runMessage msg (ThePhantomShop attrs) = runQueueT $ ThePhantomShop <$> liftRunMessage msg attrs
