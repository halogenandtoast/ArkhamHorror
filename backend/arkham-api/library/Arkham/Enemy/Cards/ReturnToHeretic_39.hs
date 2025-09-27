module Arkham.Enemy.Cards.ReturnToHeretic_39 (returnToHeretic_39) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ReturnToHeretic_39 = ReturnToHeretic_39 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

returnToHeretic_39 :: EnemyCard ReturnToHeretic_39
returnToHeretic_39 = enemy ReturnToHeretic_39 Cards.returnToHeretic_39 (0, Static 1, 0) (0, 0)

instance RunMessage ReturnToHeretic_39 where
  runMessage msg (ReturnToHeretic_39 attrs) = runQueueT $ case msg of
    _ -> ReturnToHeretic_39 <$> liftRunMessage msg attrs
