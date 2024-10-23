module Arkham.Enemy.Cards.EaterOfTheDepths (eaterOfTheDepths, EaterOfTheDepths (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype EaterOfTheDepths = EaterOfTheDepths EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

eaterOfTheDepths :: EnemyCard EaterOfTheDepths
eaterOfTheDepths =
  enemyWith EaterOfTheDepths Cards.eaterOfTheDepths (5, Static 6, 0) (3, 2)
    $ spawnAtL
    ?~ SpawnAtRandomSetAsideLocation

instance HasModifiersFor EaterOfTheDepths where
  getModifiersFor target (EaterOfTheDepths a) = maybeModified a do
    guard $ isTarget a target
    depth <- getCurrentDepth
    guard $ depth > 0
    pure [EnemyEvade depth]

instance RunMessage EaterOfTheDepths where
  runMessage msg (EaterOfTheDepths attrs) =
    EaterOfTheDepths <$> runMessage msg attrs
