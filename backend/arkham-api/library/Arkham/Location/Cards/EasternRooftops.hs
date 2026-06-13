module Arkham.Location.Cards.EasternRooftops (easternRooftops) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype EasternRooftops = EasternRooftops LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easternRooftops :: LocationCard EasternRooftops
easternRooftops = location EasternRooftops Cards.easternRooftops 3 (Static 1)

-- TODO: abilities

instance RunMessage EasternRooftops where
  runMessage msg (EasternRooftops attrs) = runQueueT $ EasternRooftops <$> liftRunMessage msg attrs
