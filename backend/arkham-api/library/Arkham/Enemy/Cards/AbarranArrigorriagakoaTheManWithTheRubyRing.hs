module Arkham.Enemy.Cards.AbarranArrigorriagakoaTheManWithTheRubyRing (abarranArrigorriagakoaTheManWithTheRubyRing) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype AbarranArrigorriagakoaTheManWithTheRubyRing = AbarranArrigorriagakoaTheManWithTheRubyRing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

abarranArrigorriagakoaTheManWithTheRubyRing :: EnemyCard AbarranArrigorriagakoaTheManWithTheRubyRing
abarranArrigorriagakoaTheManWithTheRubyRing = enemy AbarranArrigorriagakoaTheManWithTheRubyRing Cards.abarranArrigorriagakoaTheManWithTheRubyRing (0, Static 1, 0) (0, 0)

instance RunMessage AbarranArrigorriagakoaTheManWithTheRubyRing where
  runMessage msg (AbarranArrigorriagakoaTheManWithTheRubyRing attrs) = runQueueT $ case msg of
    _ -> AbarranArrigorriagakoaTheManWithTheRubyRing <$> liftRunMessage msg attrs
