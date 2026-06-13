module Arkham.Act.Cards.StepsOfGiants (stepsOfGiants) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype StepsOfGiants = StepsOfGiants ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfGiants :: ActCard StepsOfGiants
stepsOfGiants = act (1, A) StepsOfGiants Cards.stepsOfGiants Nothing

-- TODO: abilities
instance HasAbilities StepsOfGiants where
  getAbilities _ = []

instance RunMessage StepsOfGiants where
  runMessage msg (StepsOfGiants attrs) = runQueueT $ StepsOfGiants <$> liftRunMessage msg attrs
