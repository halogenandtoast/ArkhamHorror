module Arkham.Location.Cards.Downtown (downtown) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Downtown = Downtown LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtown :: LocationCard Downtown
downtown = location Downtown Cards.downtown 4 (Static 1)

-- TODO: abilities

instance RunMessage Downtown where
  runMessage msg (Downtown attrs) = runQueueT $ Downtown <$> liftRunMessage msg attrs
