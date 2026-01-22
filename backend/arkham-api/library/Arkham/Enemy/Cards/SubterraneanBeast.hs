module Arkham.Enemy.Cards.SubterraneanBeast (subterraneanBeast) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SubterraneanBeast = SubterraneanBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

subterraneanBeast :: EnemyCard SubterraneanBeast
subterraneanBeast = enemy SubterraneanBeast Cards.subterraneanBeast (4, Static 4, 3) (1, 1)

instance RunMessage SubterraneanBeast where
  runMessage msg (SubterraneanBeast attrs) = runQueueT $ case msg of
    _ -> SubterraneanBeast <$> liftRunMessage msg attrs
