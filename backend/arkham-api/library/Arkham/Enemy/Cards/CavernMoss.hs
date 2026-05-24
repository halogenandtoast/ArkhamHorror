module Arkham.Enemy.Cards.CavernMoss (cavernMoss) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CavernMoss = CavernMoss EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cavernMoss :: EnemyCard CavernMoss
cavernMoss = enemy CavernMoss Cards.cavernMoss (2, Static 3, 1) (1, 0)

instance RunMessage CavernMoss where
  runMessage msg (CavernMoss attrs) = runQueueT $ case msg of
    _ -> CavernMoss <$> liftRunMessage msg attrs
