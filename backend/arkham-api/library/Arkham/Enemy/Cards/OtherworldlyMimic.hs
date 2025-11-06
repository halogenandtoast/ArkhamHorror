module Arkham.Enemy.Cards.OtherworldlyMimic (otherworldlyMimic) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype OtherworldlyMimic = OtherworldlyMimic EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

otherworldlyMimic :: EnemyCard OtherworldlyMimic
otherworldlyMimic = enemy OtherworldlyMimic Cards.otherworldlyMimic (0, Static 1, 0) (0, 0)

instance RunMessage OtherworldlyMimic where
  runMessage msg (OtherworldlyMimic attrs) = runQueueT $ case msg of
    _ -> OtherworldlyMimic <$> liftRunMessage msg attrs
