module Arkham.Enemy.Cards.EquineHybridA (equineHybridA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype EquineHybridA = EquineHybridA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

equineHybridA :: EnemyCard EquineHybridA
equineHybridA = enemy EquineHybridA Cards.equineHybridA (2, Static 3, 2) (2, 0)
