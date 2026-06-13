module Arkham.Enemy.Cards.PrimevalTerror (primevalTerror) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PrimevalTerror = PrimevalTerror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primevalTerror :: EnemyCard PrimevalTerror
primevalTerror = enemy PrimevalTerror Cards.primevalTerror (2, Static 2, 2) (2, 0)

-- TODO: abilities
instance RunMessage PrimevalTerror where
  runMessage msg (PrimevalTerror attrs) = runQueueT $ PrimevalTerror <$> liftRunMessage msg attrs
