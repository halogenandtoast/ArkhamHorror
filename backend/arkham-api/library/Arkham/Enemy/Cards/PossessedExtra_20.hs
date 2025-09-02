module Arkham.Enemy.Cards.PossessedExtra_20 (possessedExtra_20) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PossessedExtra_20 = PossessedExtra_20 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

possessedExtra_20 :: EnemyCard PossessedExtra_20
possessedExtra_20 = enemy PossessedExtra_20 Cards.possessedExtra_20 (1, Static 1, 1) (1, 0)

instance RunMessage PossessedExtra_20 where
  runMessage msg (PossessedExtra_20 attrs) = PossessedExtra_20 <$> runMessage msg attrs
