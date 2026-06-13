module Arkham.Location.Cards.StMarysHospitalTheDrownedCity (stMarysHospitalTheDrownedCity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype StMarysHospitalTheDrownedCity = StMarysHospitalTheDrownedCity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stMarysHospitalTheDrownedCity :: LocationCard StMarysHospitalTheDrownedCity
stMarysHospitalTheDrownedCity = location StMarysHospitalTheDrownedCity Cards.stMarysHospitalTheDrownedCity 3 (Static 1)

-- TODO: abilities

instance RunMessage StMarysHospitalTheDrownedCity where
  runMessage msg (StMarysHospitalTheDrownedCity attrs) = runQueueT $ StMarysHospitalTheDrownedCity <$> liftRunMessage msg attrs
