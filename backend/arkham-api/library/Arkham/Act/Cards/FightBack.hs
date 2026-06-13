module Arkham.Act.Cards.FightBack (fightBack) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FightBack = FightBack ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightBack :: ActCard FightBack
fightBack = act (1, A) FightBack Cards.fightBack Nothing

-- TODO: abilities
instance HasAbilities FightBack where
  getAbilities _ = []

instance RunMessage FightBack where
  runMessage msg (FightBack attrs) = runQueueT $ FightBack <$> liftRunMessage msg attrs
