module Arkham.Enemy.Cards.MoltingHybridB (moltingHybridB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MoltingHybridB = MoltingHybridB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moltingHybridB :: EnemyCard MoltingHybridB
moltingHybridB = enemy MoltingHybridB Cards.moltingHybridB (2, Static 3, 4) (0, 1)
