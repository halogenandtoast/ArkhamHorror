module Arkham.Enemy.Cards.CthulhuDeadAndDreaming (cthulhuDeadAndDreaming) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CthulhuDeadAndDreaming = CthulhuDeadAndDreaming EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cthulhuDeadAndDreaming :: EnemyCard CthulhuDeadAndDreaming
cthulhuDeadAndDreaming = enemy CthulhuDeadAndDreaming Cards.cthulhuDeadAndDreaming

-- TODO: abilities
instance RunMessage CthulhuDeadAndDreaming where
  runMessage msg (CthulhuDeadAndDreaming attrs) = runQueueT $ CthulhuDeadAndDreaming <$> liftRunMessage msg attrs
