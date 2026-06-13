module Arkham.Act.Cards.CarefulNavigation (carefulNavigation) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype CarefulNavigation = CarefulNavigation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carefulNavigation :: ActCard CarefulNavigation
carefulNavigation = act (1, A) CarefulNavigation Cards.carefulNavigation Nothing

-- TODO: abilities
instance HasAbilities CarefulNavigation where
  getAbilities _ = []

instance RunMessage CarefulNavigation where
  runMessage msg (CarefulNavigation attrs) = runQueueT $ CarefulNavigation <$> liftRunMessage msg attrs
