module Arkham.Enemy.Cards.CapraHybrid (capraHybrid) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CapraHybrid = CapraHybrid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

capraHybrid :: EnemyCard CapraHybrid
capraHybrid = enemy CapraHybrid Cards.capraHybrid (3, Static 5, 3) (1, 2)
