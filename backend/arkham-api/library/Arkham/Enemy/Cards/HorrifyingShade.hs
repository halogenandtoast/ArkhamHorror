module Arkham.Enemy.Cards.HorrifyingShade (horrifyingShade) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HorrifyingShade = HorrifyingShade EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

horrifyingShade :: EnemyCard HorrifyingShade
horrifyingShade = enemy HorrifyingShade Cards.horrifyingShade (3, Static 3, 3) (1, 1)

instance RunMessage HorrifyingShade where
  runMessage msg (HorrifyingShade attrs) = runQueueT $ case msg of
    _ -> HorrifyingShade <$> liftRunMessage msg attrs
