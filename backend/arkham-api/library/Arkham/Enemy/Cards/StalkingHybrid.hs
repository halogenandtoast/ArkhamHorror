module Arkham.Enemy.Cards.StalkingHybrid (stalkingHybrid) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype StalkingHybrid = StalkingHybrid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

stalkingHybrid :: EnemyCard StalkingHybrid
stalkingHybrid = enemy StalkingHybrid Cards.stalkingHybrid (3, Static 1, 3) (1, 1)

instance RunMessage StalkingHybrid where
  runMessage msg (StalkingHybrid attrs) = runQueueT $ case msg of
    _ -> StalkingHybrid <$> liftRunMessage msg attrs
