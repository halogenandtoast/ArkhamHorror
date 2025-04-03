module Arkham.Enemy.Cards.TheNamelessMadness (theNamelessMadness) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TheNamelessMadness = TheNamelessMadness EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theNamelessMadness :: EnemyCard TheNamelessMadness
theNamelessMadness = enemy TheNamelessMadness Cards.theNamelessMadness (0, Static 1, 0) (1, 1)

instance RunMessage TheNamelessMadness where
  runMessage msg (TheNamelessMadness attrs) = runQueueT $ case msg of
    _ -> TheNamelessMadness <$> liftRunMessage msg attrs
