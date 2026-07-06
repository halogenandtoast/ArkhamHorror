module Arkham.Enemy.Cards.CthulhuFierceVisageEnraged (cthulhuFierceVisageEnraged) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CthulhuFierceVisageEnraged = CthulhuFierceVisageEnraged EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cthulhuFierceVisageEnraged :: EnemyCard CthulhuFierceVisageEnraged
cthulhuFierceVisageEnraged = enemy CthulhuFierceVisageEnraged Cards.cthulhuFierceVisageEnraged

-- TODO: abilities
instance RunMessage CthulhuFierceVisageEnraged where
  runMessage msg (CthulhuFierceVisageEnraged attrs) = runQueueT $ CthulhuFierceVisageEnraged <$> liftRunMessage msg attrs
