module Arkham.Attack where

import Arkham.Prelude

import Arkham.Id
import Arkham.Strategy
import Arkham.Target

data EnemyAttackType = AttackOfOpportunity | RegularAttack
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EnemyAttackDetails = EnemyAttackDetails
  { attackTarget :: Target
  , attackEnemy :: EnemyId
  , attackType :: EnemyAttackType
  , attackDamageStrategy :: DamageStrategy
  , attackExhaustsEnemy :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

enemyAttack
  :: Targetable targetable
  => EnemyId
  -> targetable
  -> EnemyAttackDetails
enemyAttack enemyId targetable = EnemyAttackDetails
  { attackTarget = toTarget targetable
  , attackEnemy = enemyId
  , attackType = RegularAttack
  , attackDamageStrategy = DamageAny
  , attackExhaustsEnemy = False
  }

attackOfOpportunity
  :: Targetable targetable
  => EnemyId
  -> targetable
  -> EnemyAttackDetails
attackOfOpportunity enemyId targetable =
  (enemyAttack enemyId targetable) { attackType = AttackOfOpportunity }

damageStrategyL :: Lens' EnemyAttackDetails DamageStrategy
damageStrategyL =
  lens attackDamageStrategy $ \m x -> m { attackDamageStrategy = x }
