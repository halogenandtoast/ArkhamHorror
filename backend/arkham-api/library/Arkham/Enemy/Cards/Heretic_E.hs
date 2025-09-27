{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.Cards.Heretic_E ( heretic_E,) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Story.Cards qualified as Story

newtype Heretic_E = Heretic_E EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heretic_E :: EnemyCard Heretic_E
heretic_E = enemy Heretic_E Cards.heretic_E (4, Static 2, 3) (1, 1)

instance HasModifiersFor Heretic_E where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_E where
  getAbilities = hereticAbilities

instance RunMessage Heretic_E where
  runMessage = hereticRunner Story.unfinishedBusiness_F
