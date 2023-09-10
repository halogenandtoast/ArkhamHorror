module Arkham.Enemy.Cards.MindlessDancer (
  mindlessDancer,
  MindlessDancer (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype MindlessDancer = MindlessDancer EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mindlessDancer :: EnemyCard MindlessDancer
mindlessDancer =
  enemyWith
    MindlessDancer
    Cards.mindlessDancer
    (6, Static 5, 3)
    (2, 1)
    ( spawnAtL
        ?~ SpawnLocation (IncludeEmptySpace $ FarthestLocationFromYou $ locationIs Locations.emptySpace)
    )

instance HasModifiersFor MindlessDancer where
  getModifiersFor target (MindlessDancer attrs) | isTarget attrs target = pure $ toModifiers attrs [CanEnterEmptySpace]
  getModifiersFor _ _ = pure []

instance RunMessage MindlessDancer where
  runMessage msg (MindlessDancer attrs) =
    MindlessDancer <$> runMessage msg attrs
