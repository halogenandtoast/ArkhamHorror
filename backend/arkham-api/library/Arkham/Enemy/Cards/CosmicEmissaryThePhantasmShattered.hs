module Arkham.Enemy.Cards.CosmicEmissaryThePhantasmShattered (cosmicEmissaryThePhantasmShattered) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CosmicEmissaryThePhantasmShattered = CosmicEmissaryThePhantasmShattered EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryThePhantasmShattered :: EnemyCard CosmicEmissaryThePhantasmShattered
cosmicEmissaryThePhantasmShattered =
  enemy CosmicEmissaryThePhantasmShattered Cards.cosmicEmissaryThePhantasmShattered (2, Static 10, 4) (0, 3)

instance RunMessage CosmicEmissaryThePhantasmShattered where
  runMessage msg (CosmicEmissaryThePhantasmShattered attrs) =
    runQueueT $ CosmicEmissaryThePhantasmShattered <$> liftRunMessage msg attrs
