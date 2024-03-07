module Arkham.Enemy.Cards.TheUnnamable
  ( theUnnamable
  , TheUnnamable(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype TheUnnamable = TheUnnamable EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theUnnamable :: EnemyCard TheUnnamable
theUnnamable = enemyWith TheUnnamable Cards.theUnnamable (5, Static 1, 5) (2, 2) (healthL .~ Nothing)

instance RunMessage TheUnnamable where
  runMessage msg (TheUnnamable attrs) =
    TheUnnamable <$> runMessage msg attrs
