module Arkham.Enemy.Cards.HouseDealerA (houseDealerA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HouseDealerA = HouseDealerA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

houseDealerA :: EnemyCard HouseDealerA
houseDealerA = enemy HouseDealerA Cards.houseDealerA (2, Static 1, 2) (0, 1)

instance RunMessage HouseDealerA where
  runMessage msg (HouseDealerA attrs) = runQueueT $ case msg of
    _ -> HouseDealerA <$> liftRunMessage msg attrs
