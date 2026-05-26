module Arkham.Enemy.Cards.UrsineHybrid (ursineHybrid) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype UrsineHybrid = UrsineHybrid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ursineHybrid :: EnemyCard UrsineHybrid
ursineHybrid = enemy UrsineHybrid Cards.ursineHybrid (5, Static 6, 3) (3, 2)
