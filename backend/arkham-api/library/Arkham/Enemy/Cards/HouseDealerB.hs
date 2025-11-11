module Arkham.Enemy.Cards.HouseDealerB (houseDealerB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HouseDealerB = HouseDealerB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

houseDealerB :: EnemyCard HouseDealerB
houseDealerB = enemy HouseDealerB Cards.houseDealerB (2, Static 1, 2) (0, 1)

instance RunMessage HouseDealerB where
  runMessage msg (HouseDealerB attrs) = runQueueT $ case msg of
    _ -> HouseDealerB <$> liftRunMessage msg attrs
