module Arkham.Enemy.Cards.ElderThingScavenger (elderThingScavenger) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ElderThingScavenger = ElderThingScavenger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

elderThingScavenger :: EnemyCard ElderThingScavenger
elderThingScavenger = enemy ElderThingScavenger Cards.elderThingScavenger (3, Static 3, 3) (1, 1)

instance RunMessage ElderThingScavenger where
  runMessage msg (ElderThingScavenger attrs) = runQueueT $ case msg of
    _ -> ElderThingScavenger <$> liftRunMessage msg attrs
