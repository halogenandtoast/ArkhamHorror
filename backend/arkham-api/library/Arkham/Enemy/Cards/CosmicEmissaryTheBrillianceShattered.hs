module Arkham.Enemy.Cards.CosmicEmissaryTheBrillianceShattered (cosmicEmissaryTheBrillianceShattered) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CosmicEmissaryTheBrillianceShattered = CosmicEmissaryTheBrillianceShattered EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheBrillianceShattered :: EnemyCard CosmicEmissaryTheBrillianceShattered
cosmicEmissaryTheBrillianceShattered =
  enemyWith CosmicEmissaryTheBrillianceShattered Cards.cosmicEmissaryTheBrillianceShattered (3, Static 10, 3) (2, 2)
    $ asSelfLocationL ?~ "cosmicEmissaryBrilliance"

instance RunMessage CosmicEmissaryTheBrillianceShattered where
  runMessage msg (CosmicEmissaryTheBrillianceShattered attrs) =
    runQueueT $ CosmicEmissaryTheBrillianceShattered <$> liftRunMessage msg attrs
