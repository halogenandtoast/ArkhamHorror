module Arkham.Location.Cards.AerialWaterfall (aerialWaterfall) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AerialWaterfall = AerialWaterfall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aerialWaterfall :: LocationCard AerialWaterfall
aerialWaterfall = location AerialWaterfall Cards.aerialWaterfall 4 (Static 2)

-- TODO: abilities

instance RunMessage AerialWaterfall where
  runMessage msg (AerialWaterfall attrs) = runQueueT $ AerialWaterfall <$> liftRunMessage msg attrs
