module Arkham.Act.Cards.ScouringTheSpires (scouringTheSpires) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ScouringTheSpires = ScouringTheSpires ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scouringTheSpires :: ActCard ScouringTheSpires
scouringTheSpires = act (1, A) ScouringTheSpires Cards.scouringTheSpires Nothing

-- TODO: abilities
instance HasAbilities ScouringTheSpires where
  getAbilities _ = []

instance RunMessage ScouringTheSpires where
  runMessage msg (ScouringTheSpires attrs) = runQueueT $ ScouringTheSpires <$> liftRunMessage msg attrs
