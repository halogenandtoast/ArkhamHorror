module Arkham.Enemy.Cards.LupineHybridB (lupineHybridB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype LupineHybridB = LupineHybridB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lupineHybridB :: EnemyCard LupineHybridB
lupineHybridB = enemy LupineHybridB Cards.lupineHybridB (3, Static 4, 1) (1, 1)
