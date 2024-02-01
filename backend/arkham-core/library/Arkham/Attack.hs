{-# LANGUAGE TemplateHaskell #-}

module Arkham.Attack where

import Arkham.Prelude

import Arkham.Id
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Data.Aeson.TH

data EnemyAttackType = AttackOfOpportunity | RegularAttack | AlertAttack
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

data EnemyAttackDetails = EnemyAttackDetails
  { attackTarget :: Target
  , attackEnemy :: EnemyId
  , attackType :: EnemyAttackType
  , attackDamageStrategy :: DamageStrategy
  , attackExhaustsEnemy :: Bool
  , attackSource :: Source
  , attackCanBeCanceled :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

enemyAttack
  :: (Targetable target, Sourceable source, IdOf enemy ~ EnemyId, AsId enemy)
  => enemy
  -> source
  -> target
  -> EnemyAttackDetails
enemyAttack (asId -> enemyId) (toSource -> source) (toTarget -> target) =
  EnemyAttackDetails
    { attackTarget = target
    , attackEnemy = enemyId
    , attackType = RegularAttack
    , attackDamageStrategy = DamageAny
    , attackExhaustsEnemy = False
    , attackSource = source
    , attackCanBeCanceled = True
    }

attackOfOpportunity
  :: (Targetable target, Sourceable source) => EnemyId -> source -> target -> EnemyAttackDetails
attackOfOpportunity enemyId source target =
  (enemyAttack enemyId source target) {attackType = AttackOfOpportunity}

damageStrategyL :: Lens' EnemyAttackDetails DamageStrategy
damageStrategyL =
  lens attackDamageStrategy $ \m x -> m {attackDamageStrategy = x}

viaAlert :: EnemyAttackDetails -> EnemyAttackDetails
viaAlert a = a {attackType = AlertAttack}

$(deriveJSON defaultOptions ''EnemyAttackType)
$(deriveJSON defaultOptions ''EnemyAttackDetails)
