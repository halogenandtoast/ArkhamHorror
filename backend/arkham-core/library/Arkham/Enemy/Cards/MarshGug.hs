module Arkham.Enemy.Cards.MarshGug (
  marshGug,
  MarshGug (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Trait

newtype MarshGug = MarshGug EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

marshGug :: EnemyCard MarshGug
marshGug =
  enemyWith
    MarshGug
    Cards.marshGug
    (3, Static 4, 3)
    (2, 1)
    (spawnAtL ?~ SpawnAt (LocationWithTrait Bayou))

instance RunMessage MarshGug where
  runMessage msg (MarshGug attrs) = MarshGug <$> runMessage msg attrs
