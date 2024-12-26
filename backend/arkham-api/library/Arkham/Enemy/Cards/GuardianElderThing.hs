module Arkham.Enemy.Cards.GuardianElderThing (guardianElderThing) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype GuardianElderThing = GuardianElderThing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

guardianElderThing :: EnemyCard GuardianElderThing
guardianElderThing = enemy GuardianElderThing Cards.guardianElderThing (3, Static 4, 1) (1, 1)

instance RunMessage GuardianElderThing where
  runMessage msg (GuardianElderThing attrs) = runQueueT $ case msg of
    _ -> GuardianElderThing <$> liftRunMessage msg attrs
