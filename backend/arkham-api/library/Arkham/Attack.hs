{-# LANGUAGE TemplateHaskell #-}

module Arkham.Attack (module Arkham.Attack, module Arkham.Attack.Types) where

import Arkham.Attack.Types
import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Arkham.Strategy
import Arkham.Target

enemyAttack
  :: (Targetable target, Sourceable source, IdOf enemy ~ EnemyId, AsId enemy)
  => enemy
  -> source
  -> target
  -> EnemyAttackDetails
enemyAttack (asId -> enemyId) (toSource -> source) (toTarget -> target) =
  EnemyAttackDetails
    { attackTarget = target
    , attackOriginalTarget = target
    , attackEnemy = enemyId
    , attackType = RegularAttack
    , attackDamageStrategy = DamageAny
    , attackExhaustsEnemy = False
    , attackSource = source
    , attackCanBeCanceled = True
    , attackAfter = []
    , attackDamaged = mempty
    }

attackOfOpportunity
  :: (Targetable target, Sourceable source)
  => EnemyId
  -> source
  -> target
  -> EnemyAttackDetails
attackOfOpportunity enemyId source target = do
  (enemyAttack enemyId source target) {attackType = AttackOfOpportunity}

viaAlert :: EnemyAttackDetails -> EnemyAttackDetails
viaAlert a = a {attackType = AlertAttack}
