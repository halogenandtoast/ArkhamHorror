module Arkham.Enemy.Cards.ReawakenedElderThing (reawakenedElderThing) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ReawakenedElderThing = ReawakenedElderThing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

reawakenedElderThing :: EnemyCard ReawakenedElderThing
reawakenedElderThing = enemy ReawakenedElderThing Cards.reawakenedElderThing (2, Static 3, 1) (1, 1)

instance RunMessage ReawakenedElderThing where
  runMessage msg (ReawakenedElderThing attrs) = runQueueT $ case msg of
    _ -> ReawakenedElderThing <$> liftRunMessage msg attrs
