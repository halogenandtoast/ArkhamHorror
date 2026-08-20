{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.Cards.TheCircleUndone.TheWagesOfSin.Heretic_E (heretic_E) where

import Arkham.Enemy.CardDefs.TheCircleUndone.TheWagesOfSin qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Scenarios.TheCircleUndone.TheWagesOfSin.Helpers
import Arkham.Story.CardDefs.TheCircleUndone.TheWagesOfSin qualified as Story

newtype Heretic_E = Heretic_E EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heretic_E :: EnemyCard Heretic_E
heretic_E = enemy Heretic_E Cards.heretic_E

instance HasModifiersFor Heretic_E where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_E where
  getAbilities = hereticAbilities

instance RunMessage Heretic_E where
  runMessage = hereticRunner Story.unfinishedBusiness_F
