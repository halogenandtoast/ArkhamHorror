module Arkham.Behavior.Evade where

import Arkham.Calculation
import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasQueue (HasQueue)
import Arkham.Helpers.Message (push, pushAll)
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.SkillTest.Lifted (evade)
import Arkham.Helpers.Window (checkAfter, checkWhen)
import Arkham.Window qualified as Window
import Arkham.Id
import Arkham.Message (Message, pattern EnemyEvaded, pattern EvadeEnemy)
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

{- | The 'Evadable' behavior. Used by 'Arkham.Enemy.Runner' (real enemies) and
'Arkham.EnemyLocation.Runner' (enemy-locations) and anything else that should
be evadable.
-}

-- | Push an 'EvadeEnemy' for an entity in response to the player using its
-- 'AbilityEvade' UI ability. The entity is identified by an 'EnemyId' for
-- routing through the evade subsystem (enemy-locations pass a coerced id).
pushEvadeAbility
  :: (HasQueue Message m, MonadRandom m, Sourceable s)
  => EnemyId
  -> InvestigatorId
  -> s
  -- ^ the ability source
  -> m ()
pushEvadeAbility eid iid src = do
  sid <- getRandom
  push $ EvadeEnemy sid iid eid (toSource src) Nothing SkillAgility False

-- | Resolve a 'TryEvadeEnemy' message: set up the standard evade skill test
-- using the entity's stored evade value as the default difficulty.
resolveTryEvade
  :: (ReverseQueue m, Targetable a)
  => a
  -- ^ the entity (used as the resolution target if @mTarget@ is unset)
  -> Maybe GameCalculation
  -- ^ the entity's evade value, if any; @Nothing@ falls back to 'Fixed 0'
  -> SkillTestId
  -> InvestigatorId
  -> Source
  -> Maybe Target
  -> SkillType
  -> m ()
resolveTryEvade entity defaultDifficulty sid iid source mTarget skillType = do
  let entityTarget = toTarget entity
  let target = maybe entityTarget (ProxyTarget entityTarget) mTarget
  let difficulty = fromMaybe (Fixed 0) defaultDifficulty
  evade sid iid source target skillType difficulty

-- | Build the standalone evade-skill-test 'Message' for an entity (without
-- pushing it).
mkEvadeMessage
  :: Targetable a
  => a
  -> GameCalculation
  -> SkillTestId
  -> InvestigatorId
  -> Source
  -> Maybe Target
  -> SkillType
  -> Message
mkEvadeMessage entity difficulty sid iid source mTarget skillType =
  let entityTarget = toTarget entity
      target = maybe entityTarget (ProxyTarget entityTarget) mTarget
   in Msg.evade sid iid source target skillType difficulty

-- | Push the standard successful-evade cascade:
-- @when SuccessfulEvadeEnemy@, @EnemyEvaded@, @after SuccessfulEvadeEnemy@.
pushSuccessfulEvade
  :: (HasGame m, HasQueue Message m)
  => InvestigatorId -> Source -> EnemyId -> Int -> m ()
pushSuccessfulEvade iid source eid n = do
  whenMsg <- checkWhen $ Window.SuccessfulEvadeEnemy iid source eid n
  afterMsg <- checkAfter $ Window.SuccessfulEvadeEnemy iid source eid n
  pushAll [whenMsg, EnemyEvaded iid eid, afterMsg]
