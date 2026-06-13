module Arkham.Act.Cards.BanishHim (banishHim) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype BanishHim = BanishHim ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banishHim :: ActCard BanishHim
banishHim = act (1, A) BanishHim Cards.banishHim Nothing

-- TODO: abilities
instance HasAbilities BanishHim where
  getAbilities _ = []

instance RunMessage BanishHim where
  runMessage msg (BanishHim attrs) = runQueueT $ BanishHim <$> liftRunMessage msg attrs
