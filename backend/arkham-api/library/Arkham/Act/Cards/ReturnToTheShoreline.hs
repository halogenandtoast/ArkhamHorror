module Arkham.Act.Cards.ReturnToTheShoreline (returnToTheShoreline) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ReturnToTheShoreline = ReturnToTheShoreline ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToTheShoreline :: ActCard ReturnToTheShoreline
returnToTheShoreline = act (3, A) ReturnToTheShoreline Cards.returnToTheShoreline Nothing

-- TODO: abilities
instance HasAbilities ReturnToTheShoreline where
  getAbilities _ = []

instance RunMessage ReturnToTheShoreline where
  runMessage msg (ReturnToTheShoreline attrs) = runQueueT $ ReturnToTheShoreline <$> liftRunMessage msg attrs
