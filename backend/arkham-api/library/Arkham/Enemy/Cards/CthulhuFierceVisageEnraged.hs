module Arkham.Enemy.Cards.CthulhuFierceVisageEnraged (cthulhuFierceVisageEnraged) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CthulhuFierceVisageEnraged = CthulhuFierceVisageEnraged EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuFierceVisageEnraged :: EnemyCard CthulhuFierceVisageEnraged
cthulhuFierceVisageEnraged = enemy CthulhuFierceVisageEnraged Cards.cthulhuFierceVisageEnraged (0, Static 1, 0) (0, 1)

-- TODO: abilities
instance RunMessage CthulhuFierceVisageEnraged where
  runMessage msg (CthulhuFierceVisageEnraged attrs) = runQueueT $ CthulhuFierceVisageEnraged <$> liftRunMessage msg attrs
