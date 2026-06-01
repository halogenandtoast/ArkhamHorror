module Arkham.Enemy.Cards.CosmicEmissaryThePhantasm (cosmicEmissaryThePhantasm) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CosmicEmissaryThePhantasm = CosmicEmissaryThePhantasm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryThePhantasm :: EnemyCard CosmicEmissaryThePhantasm
cosmicEmissaryThePhantasm =
  enemyWith CosmicEmissaryThePhantasm Cards.cosmicEmissaryThePhantasm (2, Static 0, 4) (1, 1)
    $ healthL .~ Nothing

instance RunMessage CosmicEmissaryThePhantasm where
  runMessage msg (CosmicEmissaryThePhantasm attrs) =
    runQueueT $ CosmicEmissaryThePhantasm <$> liftRunMessage msg attrs
