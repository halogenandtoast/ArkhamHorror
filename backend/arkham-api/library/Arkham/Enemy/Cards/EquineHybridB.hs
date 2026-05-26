module Arkham.Enemy.Cards.EquineHybridB (equineHybridB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype EquineHybridB = EquineHybridB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

equineHybridB :: EnemyCard EquineHybridB
equineHybridB = enemy EquineHybridB Cards.equineHybridB (2, Static 3, 2) (2, 0)
