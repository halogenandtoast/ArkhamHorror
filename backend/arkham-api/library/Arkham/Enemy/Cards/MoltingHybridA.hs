module Arkham.Enemy.Cards.MoltingHybridA (moltingHybridA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MoltingHybridA = MoltingHybridA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moltingHybridA :: EnemyCard MoltingHybridA
moltingHybridA = enemy MoltingHybridA Cards.moltingHybridA (2, Static 3, 4) (0, 1)
