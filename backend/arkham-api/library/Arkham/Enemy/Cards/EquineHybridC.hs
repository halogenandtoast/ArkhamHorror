module Arkham.Enemy.Cards.EquineHybridC (equineHybridC) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype EquineHybridC = EquineHybridC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

equineHybridC :: EnemyCard EquineHybridC
equineHybridC = enemy EquineHybridC Cards.equineHybridC (2, Static 3, 2) (2, 0)
