module Arkham.Enemy.Cards.CosmicEmissaryTheMiasma (cosmicEmissaryTheMiasma) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CosmicEmissaryTheMiasma = CosmicEmissaryTheMiasma EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheMiasma :: EnemyCard CosmicEmissaryTheMiasma
cosmicEmissaryTheMiasma =
  enemyWith CosmicEmissaryTheMiasma Cards.cosmicEmissaryTheMiasma (4, Static 0, 2) (2, 0)
    $ healthL .~ Nothing

instance RunMessage CosmicEmissaryTheMiasma where
  runMessage msg (CosmicEmissaryTheMiasma attrs) =
    runQueueT $ CosmicEmissaryTheMiasma <$> liftRunMessage msg attrs
