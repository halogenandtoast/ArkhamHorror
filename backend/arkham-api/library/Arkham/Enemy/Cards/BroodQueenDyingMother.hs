module Arkham.Enemy.Cards.BroodQueenDyingMother (broodQueenDyingMother) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BroodQueenDyingMother = BroodQueenDyingMother EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodQueenDyingMother :: EnemyCard BroodQueenDyingMother
broodQueenDyingMother = enemy BroodQueenDyingMother Cards.broodQueenDyingMother (1, Static 5, 3) (2, 2)

instance RunMessage BroodQueenDyingMother where
  runMessage msg (BroodQueenDyingMother attrs) = runQueueT $ case msg of
    _ -> BroodQueenDyingMother <$> liftRunMessage msg attrs
