module Arkham.Attack (module Arkham.Attack, module Arkham.Attack.Types) where

import Arkham.Attack.Types
import Arkham.Classes.HasGame
import Arkham.Helpers.Modifiers (ModifierType (..), hasModifier)
import Arkham.Id
import Arkham.Enemy.Types (EnemyAttrs)
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
    { attackTarget = SingleAttackTarget target
    , attackOriginalTarget = SingleAttackTarget target
    , attackEnemy = enemyId
    , attackType = RegularAttack
    , attackDamageStrategy = DamageAny
    , attackExhaustsEnemy = False
    , attackSource = source
    , attackCanBeCanceled = True
    , attackAfter = []
    , attackDamaged = mempty
    , attackDealDamage = True
    , attackDespiteExhausted = False
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

attackIsValid :: HasGame m => EnemyAttackDetails -> EnemyAttrs -> m Bool
attackIsValid details attrs = do
  if attrs.ready || details.despiteExhausted
    then pure True
    else case details.kind of
      RetaliateAttack -> hasModifier details.enemy CanRetaliateWhileExhausted
      _ -> pure False
