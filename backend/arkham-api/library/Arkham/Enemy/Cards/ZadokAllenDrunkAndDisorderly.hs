module Arkham.Enemy.Cards.ZadokAllenDrunkAndDisorderly
  ( zadokAllenDrunkAndDisorderly
  , ZadokAllenDrunkAndDisorderly(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype ZadokAllenDrunkAndDisorderly = ZadokAllenDrunkAndDisorderly EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

zadokAllenDrunkAndDisorderly :: EnemyCard ZadokAllenDrunkAndDisorderly
zadokAllenDrunkAndDisorderly = enemy ZadokAllenDrunkAndDisorderly Cards.zadokAllenDrunkAndDisorderly (4, Static 5, 2) (2, 0)

instance RunMessage ZadokAllenDrunkAndDisorderly where
  runMessage msg (ZadokAllenDrunkAndDisorderly attrs) =
    ZadokAllenDrunkAndDisorderly <$> runMessage msg attrs
