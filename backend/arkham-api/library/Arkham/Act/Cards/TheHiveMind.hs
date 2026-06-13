module Arkham.Act.Cards.TheHiveMind (theHiveMind) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheHiveMind = TheHiveMind ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHiveMind :: ActCard TheHiveMind
theHiveMind = act (2, A) TheHiveMind Cards.theHiveMind Nothing

-- TODO: abilities
instance HasAbilities TheHiveMind where
  getAbilities _ = []

instance RunMessage TheHiveMind where
  runMessage msg (TheHiveMind attrs) = runQueueT $ TheHiveMind <$> liftRunMessage msg attrs
