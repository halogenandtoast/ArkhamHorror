module Arkham.Location.Cards.EasttownTheDrownedCity (easttownTheDrownedCity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype EasttownTheDrownedCity = EasttownTheDrownedCity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttownTheDrownedCity :: LocationCard EasttownTheDrownedCity
easttownTheDrownedCity = location EasttownTheDrownedCity Cards.easttownTheDrownedCity 3 (Static 1)

-- TODO: abilities

instance RunMessage EasttownTheDrownedCity where
  runMessage msg (EasttownTheDrownedCity attrs) = runQueueT $ EasttownTheDrownedCity <$> liftRunMessage msg attrs
