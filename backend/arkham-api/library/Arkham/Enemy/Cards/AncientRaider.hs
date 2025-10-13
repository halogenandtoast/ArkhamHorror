module Arkham.Enemy.Cards.AncientRaider (ancientRaider) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype AncientRaider = AncientRaider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ancientRaider :: EnemyCard AncientRaider
ancientRaider = enemy AncientRaider Cards.ancientRaider (3, Static 3, 2) (1, 0)

instance RunMessage AncientRaider where
  runMessage msg (AncientRaider attrs) = runQueueT $ case msg of
    _ -> AncientRaider <$> liftRunMessage msg attrs
