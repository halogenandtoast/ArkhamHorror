module Arkham.Location.Cards.RivertownTheDrownedCity (rivertownTheDrownedCity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RivertownTheDrownedCity = RivertownTheDrownedCity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertownTheDrownedCity :: LocationCard RivertownTheDrownedCity
rivertownTheDrownedCity = location RivertownTheDrownedCity Cards.rivertownTheDrownedCity 2 (Static 1)

-- TODO: abilities

instance RunMessage RivertownTheDrownedCity where
  runMessage msg (RivertownTheDrownedCity attrs) = runQueueT $ RivertownTheDrownedCity <$> liftRunMessage msg attrs
