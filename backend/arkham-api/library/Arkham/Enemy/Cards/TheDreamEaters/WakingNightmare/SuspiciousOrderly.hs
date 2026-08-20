module Arkham.Enemy.Cards.TheDreamEaters.WakingNightmare.SuspiciousOrderly (suspiciousOrderly) where

import Arkham.Classes
import Arkham.Enemy.CardDefs.TheDreamEaters.WakingNightmare qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype SuspiciousOrderly = SuspiciousOrderly EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

suspiciousOrderly :: EnemyCard SuspiciousOrderly
suspiciousOrderly =
  enemyWith
    SuspiciousOrderly
    Cards.suspiciousOrderly
    (\a -> a {enemyFight = Nothing, enemyHealth = Nothing})

instance HasModifiersFor SuspiciousOrderly where
  getModifiersFor (SuspiciousOrderly a) = do
    modifySelf a [CannotAttack, CannotBeDamaged, CannotBeAttacked]
    modifySelect a (investigatorEngagedWith a) [CannotInvestigate]

instance RunMessage SuspiciousOrderly where
  runMessage msg (SuspiciousOrderly attrs) =
    SuspiciousOrderly <$> runMessage msg attrs
