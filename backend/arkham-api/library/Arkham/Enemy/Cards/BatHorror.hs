module Arkham.Enemy.Cards.BatHorror (batHorror) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BatHorror = BatHorror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

batHorror :: EnemyCard BatHorror
batHorror =
  enemy BatHorror Cards.batHorror (3, Static 4, 4) (1, 1)

instance RunMessage BatHorror where
  runMessage msg (BatHorror attrs) = BatHorror <$> runMessage msg attrs
