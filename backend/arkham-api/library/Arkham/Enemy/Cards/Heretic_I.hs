{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.Cards.Heretic_I (heretic_I) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Story.Cards qualified as Story

newtype Heretic_I = Heretic_I EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heretic_I :: EnemyCard Heretic_I
heretic_I = enemy Heretic_I Cards.heretic_I (4, Static 2, 3) (1, 1)

instance HasModifiersFor Heretic_I where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_I where
  getAbilities = hereticAbilities

instance RunMessage Heretic_I where
  runMessage = hereticRunner Story.unfinishedBusiness_J
