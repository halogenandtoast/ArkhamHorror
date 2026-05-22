module Arkham.Enemy.Cards.BroodSoldier (broodSoldier) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BroodSoldier = BroodSoldier EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodSoldier :: EnemyCard BroodSoldier
broodSoldier = enemy BroodSoldier Cards.broodSoldier (3, Static 2, 3) (1, 1)

instance RunMessage BroodSoldier where
  runMessage msg (BroodSoldier attrs) = runQueueT $ case msg of
    _ -> BroodSoldier <$> liftRunMessage msg attrs
