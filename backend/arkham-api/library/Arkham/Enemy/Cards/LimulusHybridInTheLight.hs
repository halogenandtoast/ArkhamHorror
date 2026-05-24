module Arkham.Enemy.Cards.LimulusHybridInTheLight (limulusHybridInTheLight) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype LimulusHybridInTheLight = LimulusHybridInTheLight EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

limulusHybridInTheLight :: EnemyCard LimulusHybridInTheLight
limulusHybridInTheLight = enemy LimulusHybridInTheLight Cards.limulusHybridInTheLight (3, Static 5, 4) (1, 2)

instance RunMessage LimulusHybridInTheLight where
  runMessage msg (LimulusHybridInTheLight attrs) = runQueueT $ case msg of
    _ -> LimulusHybridInTheLight <$> liftRunMessage msg attrs
