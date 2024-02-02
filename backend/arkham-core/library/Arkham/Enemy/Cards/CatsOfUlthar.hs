module Arkham.Enemy.Cards.CatsOfUlthar (catsOfUlthar, CatsOfUlthar (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype CatsOfUlthar = CatsOfUlthar EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

catsOfUlthar :: EnemyCard CatsOfUlthar
catsOfUlthar = enemy CatsOfUlthar Cards.catsOfUlthar (1, Static 1, 1) (1, 0)

instance RunMessage CatsOfUlthar where
  runMessage msg (CatsOfUlthar attrs) =
    CatsOfUlthar <$> runMessage msg attrs
