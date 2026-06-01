module Arkham.Enemy.Cards.CosmicEmissaryTheBrilliance (cosmicEmissaryTheBrilliance) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CosmicEmissaryTheBrilliance = CosmicEmissaryTheBrilliance EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheBrilliance :: EnemyCard CosmicEmissaryTheBrilliance
cosmicEmissaryTheBrilliance =
  enemyWith CosmicEmissaryTheBrilliance Cards.cosmicEmissaryTheBrilliance (3, Static 0, 3) (1, 1)
    $ healthL .~ Nothing

instance RunMessage CosmicEmissaryTheBrilliance where
  runMessage msg (CosmicEmissaryTheBrilliance attrs) =
    runQueueT $ CosmicEmissaryTheBrilliance <$> liftRunMessage msg attrs
