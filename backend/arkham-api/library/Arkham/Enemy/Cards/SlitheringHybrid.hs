module Arkham.Enemy.Cards.SlitheringHybrid (slitheringHybrid) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SlitheringHybrid = SlitheringHybrid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slitheringHybrid :: EnemyCard SlitheringHybrid
slitheringHybrid = enemy SlitheringHybrid Cards.slitheringHybrid (3, Static 2, 2) (0, 2)
