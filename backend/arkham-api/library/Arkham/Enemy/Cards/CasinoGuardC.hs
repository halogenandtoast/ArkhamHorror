module Arkham.Enemy.Cards.CasinoGuardC (casinoGuardC) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CasinoGuardC = CasinoGuardC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

casinoGuardC :: EnemyCard CasinoGuardC
casinoGuardC = enemy CasinoGuardC Cards.casinoGuardC (3, Static 3, 2) (2, 0)

instance RunMessage CasinoGuardC where
  runMessage msg (CasinoGuardC attrs) = runQueueT $ case msg of
    _ -> CasinoGuardC <$> liftRunMessage msg attrs
