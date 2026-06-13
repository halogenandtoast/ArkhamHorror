module Arkham.Location.Cards.FleshyPathsEasternBurrows (fleshyPathsEasternBurrows) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FleshyPathsEasternBurrows = FleshyPathsEasternBurrows LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshyPathsEasternBurrows :: LocationCard FleshyPathsEasternBurrows
fleshyPathsEasternBurrows = location FleshyPathsEasternBurrows Cards.fleshyPathsEasternBurrows 2 (Static 1)

-- TODO: abilities

instance RunMessage FleshyPathsEasternBurrows where
  runMessage msg (FleshyPathsEasternBurrows attrs) = runQueueT $ FleshyPathsEasternBurrows <$> liftRunMessage msg attrs
