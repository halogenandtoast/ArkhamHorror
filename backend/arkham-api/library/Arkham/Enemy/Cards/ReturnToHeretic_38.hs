{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.Cards.ReturnToHeretic_38 (returnToHeretic_38) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Story.Cards qualified as Story

newtype ReturnToHeretic_38 = ReturnToHeretic_38 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToHeretic_38 :: EnemyCard ReturnToHeretic_38
returnToHeretic_38 = enemy ReturnToHeretic_38 Cards.returnToHeretic_38 (4, Static 2, 3) (1, 1)

instance HasModifiersFor ReturnToHeretic_38 where
  getModifiersFor = hereticModifiers

instance HasAbilities ReturnToHeretic_38 where
  getAbilities = hereticAbilities

instance RunMessage ReturnToHeretic_38 where
  runMessage = hereticRunner Story.returnToUnfinishedBusiness_38
