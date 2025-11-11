module Arkham.Enemy.Cards.AbarranArrigorriagakoaAbarranUnleashed (abarranArrigorriagakoaAbarranUnleashed) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype AbarranArrigorriagakoaAbarranUnleashed = AbarranArrigorriagakoaAbarranUnleashed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

abarranArrigorriagakoaAbarranUnleashed :: EnemyCard AbarranArrigorriagakoaAbarranUnleashed
abarranArrigorriagakoaAbarranUnleashed = enemy AbarranArrigorriagakoaAbarranUnleashed Cards.abarranArrigorriagakoaAbarranUnleashed (4, PerPlayer 4, 4) (2, 2)

instance RunMessage AbarranArrigorriagakoaAbarranUnleashed where
  runMessage msg (AbarranArrigorriagakoaAbarranUnleashed attrs) = runQueueT $ case msg of
    _ -> AbarranArrigorriagakoaAbarranUnleashed <$> liftRunMessage msg attrs
