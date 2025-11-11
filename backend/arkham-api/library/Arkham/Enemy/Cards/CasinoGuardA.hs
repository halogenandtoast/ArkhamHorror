module Arkham.Enemy.Cards.CasinoGuardA (casinoGuardA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CasinoGuardA = CasinoGuardA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

casinoGuardA :: EnemyCard CasinoGuardA
casinoGuardA = enemy CasinoGuardA Cards.casinoGuardA (3, Static 3, 2) (2, 0)

instance RunMessage CasinoGuardA where
  runMessage msg (CasinoGuardA attrs) = runQueueT $ case msg of
    _ -> CasinoGuardA <$> liftRunMessage msg attrs
