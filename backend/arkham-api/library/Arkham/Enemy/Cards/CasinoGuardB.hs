module Arkham.Enemy.Cards.CasinoGuardB (casinoGuardB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CasinoGuardB = CasinoGuardB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

casinoGuardB :: EnemyCard CasinoGuardB
casinoGuardB = enemy CasinoGuardB Cards.casinoGuardB (3, Static 3, 2) (2, 0)

instance RunMessage CasinoGuardB where
  runMessage msg (CasinoGuardB attrs) = runQueueT $ case msg of
    _ -> CasinoGuardB <$> liftRunMessage msg attrs
