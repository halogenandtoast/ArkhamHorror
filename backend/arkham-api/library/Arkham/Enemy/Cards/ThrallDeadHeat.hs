module Arkham.Enemy.Cards.ThrallDeadHeat (thrallDeadHeat) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ThrallDeadHeat = ThrallDeadHeat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thrallDeadHeat :: EnemyCard ThrallDeadHeat
thrallDeadHeat = enemy ThrallDeadHeat Cards.thrallDeadHeat (2, Static 2, 1) (1, 0)

instance RunMessage ThrallDeadHeat where
  runMessage msg (ThrallDeadHeat attrs) = runQueueT $ case msg of
    _ -> ThrallDeadHeat <$> liftRunMessage msg attrs
