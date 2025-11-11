module Arkham.Enemy.Cards.FortunesShieldA (fortunesShieldA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype FortunesShieldA = FortunesShieldA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fortunesShieldA :: EnemyCard FortunesShieldA
fortunesShieldA = enemy FortunesShieldA Cards.fortunesShieldA (2, Static 4, 3) (1, 1)

instance RunMessage FortunesShieldA where
  runMessage msg (FortunesShieldA attrs) = runQueueT $ case msg of
    _ -> FortunesShieldA <$> liftRunMessage msg attrs
