module Arkham.Location.Cards.MagneticSpires (magneticSpires) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MagneticSpires = MagneticSpires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magneticSpires :: LocationCard MagneticSpires
magneticSpires = location MagneticSpires Cards.magneticSpires 2 (Static 3)

-- TODO: abilities

instance RunMessage MagneticSpires where
  runMessage msg (MagneticSpires attrs) = runQueueT $ MagneticSpires <$> liftRunMessage msg attrs
