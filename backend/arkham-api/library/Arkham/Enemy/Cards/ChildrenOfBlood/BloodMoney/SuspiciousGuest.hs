module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.SuspiciousGuest (suspiciousGuest) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SuspiciousGuest = SuspiciousGuest EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspiciousGuest :: EnemyCard SuspiciousGuest
suspiciousGuest = enemy SuspiciousGuest Cards.suspiciousGuest

instance RunMessage SuspiciousGuest where
  runMessage msg (SuspiciousGuest attrs) = SuspiciousGuest <$> runMessage msg attrs
