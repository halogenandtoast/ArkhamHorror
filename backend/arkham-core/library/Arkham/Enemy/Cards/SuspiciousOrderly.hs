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
  deriving anyclass (IsEnemy)
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
  getModifiersFor target (SuspiciousOrderly attrs) | isTarget attrs target = do
    pure $ toModifiers attrs [CannotAttack, CannotBeDamaged]
  getModifiersFor (InvestigatorTarget _) (SuspiciousOrderly attrs) = do
    pure $ toModifiers attrs [CannotFight (EnemyWithId $ toId attrs)]
  getModifiersFor _ _ = pure []

instance RunMessage SuspiciousOrderly where
  runMessage msg (SuspiciousOrderly attrs) =
    SuspiciousOrderly <$> runMessage msg attrs
