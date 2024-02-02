module Arkham.Enemy.Cards.Heretic_C (
  heretic_C,
  Heretic_C (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Story.Cards qualified as Story

newtype Heretic_C = Heretic_C EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

heretic_C :: EnemyCard Heretic_C
heretic_C = enemy Heretic_C Cards.heretic_C (4, Static 2, 3) (1, 1)

instance HasModifiersFor Heretic_C where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_C where
  getAbilities = hereticAbilities

instance RunMessage Heretic_C where
  runMessage = hereticRunner Story.unfinishedBusiness_D
