module Arkham.Location.Cards.TheDrownedCity.OneLastJob.LaBellaLunaTheDrownedCity (laBellaLunaTheDrownedCity) where

import Arkham.Location.CardDefs.TheDrownedCity.OneLastJob qualified as Cards
import Arkham.Location.Import.Lifted

newtype LaBellaLunaTheDrownedCity = LaBellaLunaTheDrownedCity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

laBellaLunaTheDrownedCity :: LocationCard LaBellaLunaTheDrownedCity
laBellaLunaTheDrownedCity = location LaBellaLunaTheDrownedCity Cards.laBellaLunaTheDrownedCity 3 (PerPlayer 2)

instance RunMessage LaBellaLunaTheDrownedCity where
  runMessage msg (LaBellaLunaTheDrownedCity attrs) =
    runQueueT $ LaBellaLunaTheDrownedCity <$> liftRunMessage msg attrs
