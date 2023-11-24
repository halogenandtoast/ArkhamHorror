module Arkham.Enemy.Cards.SuspiciousOrderly
  ( suspiciousOrderly
  , SuspiciousOrderly(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SuspiciousOrderly = SuspiciousOrderly EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

suspiciousOrderly :: EnemyCard SuspiciousOrderly
suspiciousOrderly = enemy SuspiciousOrderly Cards.suspiciousOrderly (0, Static 1, 2) (0, 0)

instance RunMessage SuspiciousOrderly where
  runMessage msg (SuspiciousOrderly attrs) =
    SuspiciousOrderly <$> runMessage msg attrs
