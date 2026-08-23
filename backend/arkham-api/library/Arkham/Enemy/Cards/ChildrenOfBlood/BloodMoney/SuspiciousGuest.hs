module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.SuspiciousGuest (suspiciousGuest) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype SuspiciousGuest = SuspiciousGuest EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

suspiciousGuest :: EnemyCard SuspiciousGuest
suspiciousGuest = enemyWith SuspiciousGuest Cards.suspiciousGuest (spawnAtL ?~ SpawnAt EmptyLocation)
