module Arkham.Enemy.Cards.FortunesDaggerA (fortunesDaggerA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype FortunesDaggerA = FortunesDaggerA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fortunesDaggerA :: EnemyCard FortunesDaggerA
fortunesDaggerA = enemy FortunesDaggerA Cards.fortunesDaggerA (4, Static 3, 2) (1, 1)

instance RunMessage FortunesDaggerA where
  runMessage msg (FortunesDaggerA attrs) = runQueueT $ case msg of
    _ -> FortunesDaggerA <$> liftRunMessage msg attrs
