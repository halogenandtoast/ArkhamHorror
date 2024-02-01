module Arkham.Enemy.Cards.Heretic_A (
  heretic_A,
  Heretic_A (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Story.Cards qualified as Story

newtype Heretic_A = Heretic_A EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

heretic_A :: EnemyCard Heretic_A
heretic_A = enemy Heretic_A Cards.heretic_A (4, Static 2, 3) (1, 1)

instance HasModifiersFor Heretic_A where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_A where
  getAbilities = hereticAbilities

instance RunMessage Heretic_A where
  runMessage = hereticRunner Story.unfinishedBusiness_B
