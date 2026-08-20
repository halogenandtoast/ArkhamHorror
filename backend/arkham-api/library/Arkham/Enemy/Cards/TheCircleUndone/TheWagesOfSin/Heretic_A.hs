{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.Cards.TheCircleUndone.TheWagesOfSin.Heretic_A (heretic_A) where

import Arkham.Enemy.CardDefs.TheCircleUndone.TheWagesOfSin qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Scenarios.TheCircleUndone.TheWagesOfSin.Helpers
import Arkham.Story.CardDefs.TheCircleUndone.TheWagesOfSin qualified as Story

newtype Heretic_A = Heretic_A EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heretic_A :: EnemyCard Heretic_A
heretic_A = enemy Heretic_A Cards.heretic_A

instance HasModifiersFor Heretic_A where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_A where
  getAbilities = hereticAbilities

instance RunMessage Heretic_A where
  runMessage = hereticRunner Story.unfinishedBusiness_B
