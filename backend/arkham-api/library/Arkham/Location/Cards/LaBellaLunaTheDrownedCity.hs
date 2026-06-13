module Arkham.Location.Cards.LaBellaLunaTheDrownedCity (laBellaLunaTheDrownedCity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LaBellaLunaTheDrownedCity = LaBellaLunaTheDrownedCity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laBellaLunaTheDrownedCity :: LocationCard LaBellaLunaTheDrownedCity
laBellaLunaTheDrownedCity = location LaBellaLunaTheDrownedCity Cards.laBellaLunaTheDrownedCity 3 (Static 2)

instance RunMessage LaBellaLunaTheDrownedCity where
  runMessage msg (LaBellaLunaTheDrownedCity attrs) =
    runQueueT $ LaBellaLunaTheDrownedCity <$> liftRunMessage msg attrs
