module Arkham.Enemy.Cards.PossessedExtra_19 (possessedExtra_19) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PossessedExtra_19 = PossessedExtra_19 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

possessedExtra_19 :: EnemyCard PossessedExtra_19
possessedExtra_19 = enemy PossessedExtra_19 Cards.possessedExtra_19 (1, Static 1, 1) (1, 0)

instance RunMessage PossessedExtra_19 where
  runMessage msg (PossessedExtra_19 attrs) = PossessedExtra_19 <$> runMessage msg attrs
