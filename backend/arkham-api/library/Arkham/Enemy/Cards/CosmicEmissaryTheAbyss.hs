module Arkham.Enemy.Cards.CosmicEmissaryTheAbyss (cosmicEmissaryTheAbyss) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CosmicEmissaryTheAbyss = CosmicEmissaryTheAbyss EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheAbyss :: EnemyCard CosmicEmissaryTheAbyss
cosmicEmissaryTheAbyss =
  enemyWith CosmicEmissaryTheAbyss Cards.cosmicEmissaryTheAbyss (4, Static 0, 4) (0, 2)
    $ healthL .~ Nothing

instance RunMessage CosmicEmissaryTheAbyss where
  runMessage msg (CosmicEmissaryTheAbyss attrs) =
    runQueueT $ CosmicEmissaryTheAbyss <$> liftRunMessage msg attrs
