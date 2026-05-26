module Arkham.Enemy.Cards.LupineHybridC (lupineHybridC) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype LupineHybridC = LupineHybridC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lupineHybridC :: EnemyCard LupineHybridC
lupineHybridC = enemy LupineHybridC Cards.lupineHybridC (3, Static 4, 1) (1, 1)
