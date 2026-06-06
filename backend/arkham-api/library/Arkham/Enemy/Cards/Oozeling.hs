module Arkham.Enemy.Cards.Oozeling (oozeling) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Oozified))

newtype Oozeling = Oozeling EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oozeling :: EnemyCard Oozeling
oozeling =
  enemyWith Oozeling Cards.oozeling (2, Static 3, 2) (1, 0)
    $ spawnAtL
    ?~ SpawnAt (EmptyLocation <> LocationWithTrait Oozified)

instance RunMessage Oozeling where
  runMessage msg (Oozeling attrs) = Oozeling <$> runMessage msg attrs
