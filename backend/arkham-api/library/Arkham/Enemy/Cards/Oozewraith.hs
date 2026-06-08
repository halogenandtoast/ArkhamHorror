module Arkham.Enemy.Cards.Oozewraith (oozewraith) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Oozified))

newtype Oozewraith = Oozewraith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oozewraith :: EnemyCard Oozewraith
oozewraith =
  enemyWith Oozewraith Cards.oozewraith (4, Static 7, 2) (1, 1)
    $ spawnAtL
    ?~ SpawnAt (FarthestLocationFromYou $ LocationWithTrait Oozified)

instance RunMessage Oozewraith where
  runMessage msg (Oozewraith attrs) = Oozewraith <$> runMessage msg attrs
