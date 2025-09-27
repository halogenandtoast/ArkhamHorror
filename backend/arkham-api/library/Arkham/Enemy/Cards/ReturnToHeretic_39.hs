{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.Cards.ReturnToHeretic_39 (returnToHeretic_39) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Story.Cards qualified as Story

newtype ReturnToHeretic_39 = ReturnToHeretic_39 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToHeretic_39 :: EnemyCard ReturnToHeretic_39
returnToHeretic_39 = enemy ReturnToHeretic_39 Cards.returnToHeretic_39 (4, Static 2, 3) (1, 1)

instance HasModifiersFor ReturnToHeretic_39 where
  getModifiersFor = hereticModifiers

instance HasAbilities ReturnToHeretic_39 where
  getAbilities = hereticAbilities

instance RunMessage ReturnToHeretic_39 where
  runMessage = hereticRunner Story.returnToUnfinishedBusiness_39
