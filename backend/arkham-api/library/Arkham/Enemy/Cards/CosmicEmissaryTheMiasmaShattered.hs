module Arkham.Enemy.Cards.CosmicEmissaryTheMiasmaShattered (cosmicEmissaryTheMiasmaShattered) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CosmicEmissaryTheMiasmaShattered = CosmicEmissaryTheMiasmaShattered EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheMiasmaShattered :: EnemyCard CosmicEmissaryTheMiasmaShattered
cosmicEmissaryTheMiasmaShattered =
  enemy CosmicEmissaryTheMiasmaShattered Cards.cosmicEmissaryTheMiasmaShattered (4, Static 10, 2) (3, 0)

instance RunMessage CosmicEmissaryTheMiasmaShattered where
  runMessage msg (CosmicEmissaryTheMiasmaShattered attrs) =
    runQueueT $ CosmicEmissaryTheMiasmaShattered <$> liftRunMessage msg attrs
