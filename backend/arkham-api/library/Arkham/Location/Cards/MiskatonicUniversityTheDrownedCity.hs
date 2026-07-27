module Arkham.Location.Cards.MiskatonicUniversityTheDrownedCity (miskatonicUniversityTheDrownedCity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MiskatonicUniversityTheDrownedCity = MiskatonicUniversityTheDrownedCity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

miskatonicUniversityTheDrownedCity :: LocationCard MiskatonicUniversityTheDrownedCity
miskatonicUniversityTheDrownedCity = location MiskatonicUniversityTheDrownedCity Cards.miskatonicUniversityTheDrownedCity 5 (Static 1)

-- TODO: abilities

instance RunMessage MiskatonicUniversityTheDrownedCity where
  runMessage msg (MiskatonicUniversityTheDrownedCity attrs) = runQueueT $ MiskatonicUniversityTheDrownedCity <$> liftRunMessage msg attrs
