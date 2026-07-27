module Arkham.Location.Cards.WesternRooftops (westernRooftops) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WesternRooftops = WesternRooftops LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

westernRooftops :: LocationCard WesternRooftops
westernRooftops = location WesternRooftops Cards.westernRooftops 3 (Static 1)

-- TODO: abilities

instance RunMessage WesternRooftops where
  runMessage msg (WesternRooftops attrs) = runQueueT $ WesternRooftops <$> liftRunMessage msg attrs
