module Arkham.Location.Cards.GraspingCorridor (graspingCorridor) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GraspingCorridor = GraspingCorridor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graspingCorridor :: LocationCard GraspingCorridor
graspingCorridor = location GraspingCorridor Cards.graspingCorridor 2 (Static 2)

-- TODO: abilities

instance RunMessage GraspingCorridor where
  runMessage msg (GraspingCorridor attrs) = runQueueT $ GraspingCorridor <$> liftRunMessage msg attrs
