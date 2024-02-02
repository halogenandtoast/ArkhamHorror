module Arkham.Enemy.Cards.Heretic_G (
  heretic_G,
  Heretic_G (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Story.Cards qualified as Story

newtype Heretic_G = Heretic_G EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

heretic_G :: EnemyCard Heretic_G
heretic_G = enemy Heretic_G Cards.heretic_G (4, Static 2, 3) (1, 1)

instance HasModifiersFor Heretic_G where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_G where
  getAbilities = hereticAbilities

instance RunMessage Heretic_G where
  runMessage = hereticRunner Story.unfinishedBusiness_H
