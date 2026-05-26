module Arkham.Enemy.Cards.LupineHybridA (lupineHybridA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype LupineHybridA = LupineHybridA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lupineHybridA :: EnemyCard LupineHybridA
lupineHybridA = enemy LupineHybridA Cards.lupineHybridA (3, Static 4, 1) (1, 1)
