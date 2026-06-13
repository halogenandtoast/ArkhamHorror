module Arkham.Location.Cards.DazzlingSkyline (dazzlingSkyline) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DazzlingSkyline = DazzlingSkyline LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dazzlingSkyline :: LocationCard DazzlingSkyline
dazzlingSkyline = location DazzlingSkyline Cards.dazzlingSkyline 1 (Static 1)

-- TODO: abilities

instance RunMessage DazzlingSkyline where
  runMessage msg (DazzlingSkyline attrs) = runQueueT $ DazzlingSkyline <$> liftRunMessage msg attrs
