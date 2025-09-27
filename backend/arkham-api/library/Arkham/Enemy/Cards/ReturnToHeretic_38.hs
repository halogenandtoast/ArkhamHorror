module Arkham.Enemy.Cards.ReturnToHeretic_38 (returnToHeretic_38) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ReturnToHeretic_38 = ReturnToHeretic_38 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

returnToHeretic_38 :: EnemyCard ReturnToHeretic_38
returnToHeretic_38 = enemy ReturnToHeretic_38 Cards.returnToHeretic_38 (0, Static 1, 0) (0, 0)

instance RunMessage ReturnToHeretic_38 where
  runMessage msg (ReturnToHeretic_38 attrs) = runQueueT $ case msg of
    _ -> ReturnToHeretic_38 <$> liftRunMessage msg attrs
