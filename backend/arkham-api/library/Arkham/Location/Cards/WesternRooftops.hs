module Arkham.Location.Cards.WesternRooftops (westernRooftops) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WesternRooftops = WesternRooftops LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernRooftops :: LocationCard WesternRooftops
westernRooftops = location WesternRooftops Cards.westernRooftops 3 (Static 1)

-- TODO: abilities

instance RunMessage WesternRooftops where
  runMessage msg (WesternRooftops attrs) = runQueueT $ WesternRooftops <$> liftRunMessage msg attrs
