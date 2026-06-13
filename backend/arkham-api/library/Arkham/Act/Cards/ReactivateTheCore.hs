module Arkham.Act.Cards.ReactivateTheCore (reactivateTheCore) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ReactivateTheCore = ReactivateTheCore ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reactivateTheCore :: ActCard ReactivateTheCore
reactivateTheCore = act (1, A) ReactivateTheCore Cards.reactivateTheCore Nothing

-- TODO: abilities
instance HasAbilities ReactivateTheCore where
  getAbilities _ = []

instance RunMessage ReactivateTheCore where
  runMessage msg (ReactivateTheCore attrs) = runQueueT $ ReactivateTheCore <$> liftRunMessage msg attrs
