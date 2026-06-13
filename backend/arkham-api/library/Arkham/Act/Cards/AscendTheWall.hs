module Arkham.Act.Cards.AscendTheWall (ascendTheWall) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype AscendTheWall = AscendTheWall ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendTheWall :: ActCard AscendTheWall
ascendTheWall = act (1, A) AscendTheWall Cards.ascendTheWall Nothing

-- TODO: abilities
instance HasAbilities AscendTheWall where
  getAbilities _ = []

instance RunMessage AscendTheWall where
  runMessage msg (AscendTheWall attrs) = runQueueT $ AscendTheWall <$> liftRunMessage msg attrs
