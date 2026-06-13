module Arkham.Location.Cards.DrownedShanty (drownedShanty) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DrownedShanty = DrownedShanty LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drownedShanty :: LocationCard DrownedShanty
drownedShanty = location DrownedShanty Cards.drownedShanty 0 (Static 2)

-- TODO: abilities

instance RunMessage DrownedShanty where
  runMessage msg (DrownedShanty attrs) = runQueueT $ DrownedShanty <$> liftRunMessage msg attrs
