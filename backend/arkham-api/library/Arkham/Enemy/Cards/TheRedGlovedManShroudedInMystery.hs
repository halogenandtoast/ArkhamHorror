module Arkham.Enemy.Cards.TheRedGlovedManShroudedInMystery (theRedGlovedManShroudedInMystery) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TheRedGlovedManShroudedInMystery = TheRedGlovedManShroudedInMystery EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theRedGlovedManShroudedInMystery :: EnemyCard TheRedGlovedManShroudedInMystery
theRedGlovedManShroudedInMystery = enemy TheRedGlovedManShroudedInMystery Cards.theRedGlovedManShroudedInMystery (5, PerPlayer 2, 5) (1, 1)

instance RunMessage TheRedGlovedManShroudedInMystery where
  runMessage msg (TheRedGlovedManShroudedInMystery attrs) = runQueueT $ case msg of
    _ -> TheRedGlovedManShroudedInMystery <$> liftRunMessage msg attrs
