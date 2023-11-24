module Arkham.Enemy.Cards.CorruptedOrderly
  ( corruptedOrderly
  , CorruptedOrderly(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype CorruptedOrderly = CorruptedOrderly EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

corruptedOrderly :: EnemyCard CorruptedOrderly
corruptedOrderly = enemy CorruptedOrderly Cards.corruptedOrderly (2, Static 2, 2) (1, 1)

instance RunMessage CorruptedOrderly where
  runMessage msg (CorruptedOrderly attrs) =
    CorruptedOrderly <$> runMessage msg attrs
