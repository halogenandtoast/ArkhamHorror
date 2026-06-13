module Arkham.Location.Cards.TheGreatStair (theGreatStair) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheGreatStair = TheGreatStair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatStair :: LocationCard TheGreatStair
theGreatStair = location TheGreatStair Cards.theGreatStair 2 (Static 2)

-- TODO: abilities

instance RunMessage TheGreatStair where
  runMessage msg (TheGreatStair attrs) = runQueueT $ TheGreatStair <$> liftRunMessage msg attrs
