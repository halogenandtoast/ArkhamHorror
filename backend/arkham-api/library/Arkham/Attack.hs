module Arkham.Attack (module Arkham.Attack, module Arkham.Attack.Types) where

import Arkham.Attack.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query ((<=~>))
import Arkham.Enemy.Types (EnemyAttrs)
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers, hasModifier)
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
    , attackCancelled = False
    }
viaAlert :: EnemyAttackDetails -> EnemyAttackDetails
viaAlert a = a {attackType = AlertAttack}

-- | Whether @enemyId@ is allowed to attack @iid@ right now.
canBeAttackedBy :: HasGame m => EnemyId -> InvestigatorId -> m Bool
canBeAttackedBy enemyId iid = do
  mods <- getModifiers iid
  flip noneM mods \case
    CannotBeAttackedBy matcher -> enemyId <=~> matcher
    _ -> pure False

attackIsValid :: HasGame m => EnemyAttackDetails -> EnemyAttrs -> m Bool
attackIsValid details attrs = andM [readyEnough, targetIsValid]
 where
  readyEnough
    | attrs.ready || details.despiteExhausted = pure True
    | otherwise = case details.kind of
        RetaliateAttack -> hasModifier details.enemy CanRetaliateWhileExhausted
        _ -> pure False
  targetIsValid = maybe (pure True) (canBeAttackedBy details.enemy) details.investigator
