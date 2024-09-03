module Arkham.Enemy.Cards.Heretic_K (
  heretic_K,
  Heretic_K (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Story.Cards qualified as Story

newtype Metadata = Metadata {hasBeenRevealed :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Heretic_K = Heretic_K (EnemyAttrs `With` Metadata)
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heretic_K :: EnemyCard Heretic_K
heretic_K = enemy (Heretic_K . (`with` Metadata False)) Cards.heretic_K (4, Static 2, 3) (1, 1)

instance HasModifiersFor Heretic_K where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_K where
  getAbilities = hereticAbilities

instance RunMessage Heretic_K where
  runMessage = hereticRunner Story.unfinishedBusiness_L
