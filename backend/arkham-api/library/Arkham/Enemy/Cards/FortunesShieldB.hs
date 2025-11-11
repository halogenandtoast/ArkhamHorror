module Arkham.Enemy.Cards.FortunesShieldB (fortunesShieldB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype FortunesShieldB = FortunesShieldB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fortunesShieldB :: EnemyCard FortunesShieldB
fortunesShieldB = enemy FortunesShieldB Cards.fortunesShieldB (2, Static 4, 3) (1, 1)

instance RunMessage FortunesShieldB where
  runMessage msg (FortunesShieldB attrs) = runQueueT $ case msg of
    _ -> FortunesShieldB <$> liftRunMessage msg attrs
