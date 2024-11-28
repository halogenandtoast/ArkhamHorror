module Arkham.Enemy.Cards.AveryClaypoolAntarcticGuide
  ( averyClaypoolAntarcticGuide
  , AveryClaypoolAntarcticGuide(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype AveryClaypoolAntarcticGuide = AveryClaypoolAntarcticGuide EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

averyClaypoolAntarcticGuide :: EnemyCard AveryClaypoolAntarcticGuide
averyClaypoolAntarcticGuide = enemy AveryClaypoolAntarcticGuide Cards.averyClaypoolAntarcticGuide (0, Static 1, 0) (0, 0)

instance RunMessage AveryClaypoolAntarcticGuide where
  runMessage msg (AveryClaypoolAntarcticGuide attrs) = runQueueT $ case msg of
    _ -> AveryClaypoolAntarcticGuide <$> liftRunMessage msg attrs
