module Arkham.Enemy.Cards.ColorlessLarva (colorlessLarva) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ColorlessLarva = ColorlessLarva EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

colorlessLarva :: EnemyCard ColorlessLarva
colorlessLarva = enemy ColorlessLarva Cards.colorlessLarva (1, Static 1, 1) (0, 1)

instance RunMessage ColorlessLarva where
  runMessage msg (ColorlessLarva attrs) = runQueueT $ case msg of
    _ -> ColorlessLarva <$> liftRunMessage msg attrs
