{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.Cards.TheCircleUndone.TheWagesOfSin.Heretic_G (heretic_G) where

import Arkham.Enemy.CardDefs.TheCircleUndone.TheWagesOfSin qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Scenarios.TheCircleUndone.TheWagesOfSin.Helpers
import Arkham.Story.CardDefs.TheCircleUndone.TheWagesOfSin qualified as Story

newtype Heretic_G = Heretic_G EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heretic_G :: EnemyCard Heretic_G
heretic_G = enemy Heretic_G Cards.heretic_G

instance HasModifiersFor Heretic_G where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_G where
  getAbilities = hereticAbilities

instance RunMessage Heretic_G where
  runMessage = hereticRunner Story.unfinishedBusiness_H
