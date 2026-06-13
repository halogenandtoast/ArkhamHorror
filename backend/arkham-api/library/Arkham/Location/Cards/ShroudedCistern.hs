module Arkham.Location.Cards.ShroudedCistern (shroudedCistern) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ShroudedCistern = ShroudedCistern LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudedCistern :: LocationCard ShroudedCistern
shroudedCistern = location ShroudedCistern Cards.shroudedCistern 3 (Static 2)

-- TODO: abilities

instance RunMessage ShroudedCistern where
  runMessage msg (ShroudedCistern attrs) = runQueueT $ ShroudedCistern <$> liftRunMessage msg attrs
