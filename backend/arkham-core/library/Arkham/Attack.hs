module Arkham.Attack
where

import Arkham.Prelude

data EnemyAttackType = AttackOfOpportunity | RegularAttack
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
