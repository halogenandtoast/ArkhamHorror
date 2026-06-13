module Arkham.Location.Cards.FleshyPathsWesternBurrows (fleshyPathsWesternBurrows) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FleshyPathsWesternBurrows = FleshyPathsWesternBurrows LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshyPathsWesternBurrows :: LocationCard FleshyPathsWesternBurrows
fleshyPathsWesternBurrows = location FleshyPathsWesternBurrows Cards.fleshyPathsWesternBurrows 2 (Static 1)

-- TODO: abilities

instance RunMessage FleshyPathsWesternBurrows where
  runMessage msg (FleshyPathsWesternBurrows attrs) = runQueueT $ FleshyPathsWesternBurrows <$> liftRunMessage msg attrs
