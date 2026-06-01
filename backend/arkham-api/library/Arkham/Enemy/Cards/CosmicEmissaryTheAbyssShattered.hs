module Arkham.Enemy.Cards.CosmicEmissaryTheAbyssShattered (cosmicEmissaryTheAbyssShattered) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CosmicEmissaryTheAbyssShattered = CosmicEmissaryTheAbyssShattered EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheAbyssShattered :: EnemyCard CosmicEmissaryTheAbyssShattered
cosmicEmissaryTheAbyssShattered =
  enemy CosmicEmissaryTheAbyssShattered Cards.cosmicEmissaryTheAbyssShattered (4, Static 10, 4) (1, 1)

instance RunMessage CosmicEmissaryTheAbyssShattered where
  runMessage msg (CosmicEmissaryTheAbyssShattered attrs) =
    runQueueT $ CosmicEmissaryTheAbyssShattered <$> liftRunMessage msg attrs
