module Arkham.Act.Cards.DeadlySkies (deadlySkies) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype DeadlySkies = DeadlySkies ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlySkies :: ActCard DeadlySkies
deadlySkies = act (2, A) DeadlySkies Cards.deadlySkies Nothing

-- TODO: abilities
instance HasAbilities DeadlySkies where
  getAbilities _ = []

instance RunMessage DeadlySkies where
  runMessage msg (DeadlySkies attrs) = runQueueT $ DeadlySkies <$> liftRunMessage msg attrs
