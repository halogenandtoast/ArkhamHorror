module Arkham.Enemy.Cards.LimulusHybridInTheDark (limulusHybridInTheDark) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype LimulusHybridInTheDark = LimulusHybridInTheDark EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

limulusHybridInTheDark :: EnemyCard LimulusHybridInTheDark
limulusHybridInTheDark = enemy LimulusHybridInTheDark Cards.limulusHybridInTheDark (4, Static 5, 3) (2, 1)

instance RunMessage LimulusHybridInTheDark where
  runMessage msg (LimulusHybridInTheDark attrs) = runQueueT $ case msg of
    _ -> LimulusHybridInTheDark <$> liftRunMessage msg attrs
