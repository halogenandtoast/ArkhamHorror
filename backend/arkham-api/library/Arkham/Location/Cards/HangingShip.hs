module Arkham.Location.Cards.HangingShip (hangingShip) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HangingShip = HangingShip LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangingShip :: LocationCard HangingShip
hangingShip = location HangingShip Cards.hangingShip 2 (Static 4)

-- TODO: abilities

instance RunMessage HangingShip where
  runMessage msg (HangingShip attrs) = runQueueT $ HangingShip <$> liftRunMessage msg attrs
