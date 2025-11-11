module Arkham.Enemy.Cards.FortunesDaggerB (fortunesDaggerB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype FortunesDaggerB = FortunesDaggerB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fortunesDaggerB :: EnemyCard FortunesDaggerB
fortunesDaggerB = enemy FortunesDaggerB Cards.fortunesDaggerB (4, Static 3, 2) (1, 1)

instance RunMessage FortunesDaggerB where
  runMessage msg (FortunesDaggerB attrs) = runQueueT $ case msg of
    _ -> FortunesDaggerB <$> liftRunMessage msg attrs
