module Arkham.Enemy.Cards.PossessedExtra_21 (possessedExtra_21) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PossessedExtra_21 = PossessedExtra_21 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

possessedExtra_21 :: EnemyCard PossessedExtra_21
possessedExtra_21 = enemy PossessedExtra_21 Cards.possessedExtra_21 (1, Static 1, 1) (1, 0)

instance RunMessage PossessedExtra_21 where
  runMessage msg (PossessedExtra_21 attrs) = PossessedExtra_21 <$> runMessage msg attrs
