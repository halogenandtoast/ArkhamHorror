{-# LANGUAGE TemplateHaskell #-}

module Arkham.Attack where

import Arkham.Prelude

import Arkham.Classes.Entity.Source
import Arkham.Id
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Data.Aeson.TH

data EnemyAttackType = AttackOfOpportunity | RegularAttack
  deriving stock (Show, Eq, Ord)

data EnemyAttackDetails = EnemyAttackDetails
  { attackTarget :: Target
  , attackEnemy :: EnemyId
  , attackType :: EnemyAttackType
  , attackDamageStrategy :: DamageStrategy
  , attackExhaustsEnemy :: Bool
  , attackSource :: Source
  }
  deriving stock (Show, Eq, Ord)

enemyAttack
  :: (Targetable target, Sourceable source) => EnemyId -> source -> target -> EnemyAttackDetails
enemyAttack enemyId (toSource -> source) (toTarget -> target) =
  EnemyAttackDetails
    { attackTarget = target
    , attackEnemy = enemyId
    , attackType = RegularAttack
    , attackDamageStrategy = DamageAny
    , attackExhaustsEnemy = False
    , attackSource = source
    }

attackOfOpportunity
  :: (Targetable target, Sourceable source) => EnemyId -> source -> target -> EnemyAttackDetails
attackOfOpportunity enemyId source target =
  (enemyAttack enemyId source target) {attackType = AttackOfOpportunity}

damageStrategyL :: Lens' EnemyAttackDetails DamageStrategy
damageStrategyL =
  lens attackDamageStrategy $ \m x -> m {attackDamageStrategy = x}

$(deriveJSON defaultOptions ''EnemyAttackType)
$(deriveJSON defaultOptions ''EnemyAttackDetails)
