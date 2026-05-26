module Arkham.Enemy.Cards.MoltingHybridC (moltingHybridC) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MoltingHybridC = MoltingHybridC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moltingHybridC :: EnemyCard MoltingHybridC
moltingHybridC = enemy MoltingHybridC Cards.moltingHybridC (2, Static 3, 4) (0, 1)
