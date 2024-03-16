module Arkham.Enemy.Cards.GugSentinel
  ( gugSentinel
  , GugSentinel(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype GugSentinel = GugSentinel EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

gugSentinel :: EnemyCard GugSentinel
gugSentinel = enemy GugSentinel Cards.gugSentinel (5, Static 2, 3) (2, 1)

instance RunMessage GugSentinel where
  runMessage msg (GugSentinel attrs) =
    GugSentinel <$> runMessage msg attrs
