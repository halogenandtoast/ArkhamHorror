module Arkham.Location.Cards.NorthsideTheDrownedCity (northsideTheDrownedCity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype NorthsideTheDrownedCity = NorthsideTheDrownedCity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

northsideTheDrownedCity :: LocationCard NorthsideTheDrownedCity
northsideTheDrownedCity = location NorthsideTheDrownedCity Cards.northsideTheDrownedCity 4 (Static 1)

-- TODO: abilities

instance RunMessage NorthsideTheDrownedCity where
  runMessage msg (NorthsideTheDrownedCity attrs) = runQueueT $ NorthsideTheDrownedCity <$> liftRunMessage msg attrs
