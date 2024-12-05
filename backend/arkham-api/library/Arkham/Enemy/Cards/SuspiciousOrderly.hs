module Arkham.Enemy.Cards.SuspiciousOrderly (
  suspiciousOrderly,
  SuspiciousOrderly (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype SuspiciousOrderly = SuspiciousOrderly EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

suspiciousOrderly :: EnemyCard SuspiciousOrderly
suspiciousOrderly =
  enemyWith
    SuspiciousOrderly
    Cards.suspiciousOrderly
    (0, Static 1, 2)
    (0, 0)
    (\a -> a {enemyFight = Nothing, enemyHealth = Nothing})

instance HasModifiersFor SuspiciousOrderly where
  getModifiersFor (SuspiciousOrderly a) = do
    self <- modifySelf a [CannotAttack, CannotBeDamaged, CannotBeAttacked]
    investigators <- modifySelect a (investigatorEngagedWith a) [CannotInvestigate]
    pure $ self <> investigators

instance RunMessage SuspiciousOrderly where
  runMessage msg (SuspiciousOrderly attrs) =
    SuspiciousOrderly <$> runMessage msg attrs
