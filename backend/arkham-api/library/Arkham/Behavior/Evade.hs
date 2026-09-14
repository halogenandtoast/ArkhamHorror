module Arkham.Behavior.Evade where

import Arkham.Calculation
import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasQueue (HasQueue)
import {-# SOURCE #-} Arkham.GameEnv (getWindowTick)
import Arkham.Helpers.Message (push, pushAll)
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.SkillTest.Lifted (evade)
import Arkham.Helpers.Window (checkAfter, checkWhen, checkWindows, checkWindowsAt, wouldWindows)
import Arkham.Id
import Arkham.Message (Message (Do, Would), pattern EnemyEvaded, pattern EvadeEnemy)
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window

{- | The 'Evadable' behavior. Used by 'Arkham.Enemy.Runner' (real enemies) and
'Arkham.EnemyLocation.Runner' (enemy-locations) and anything else that should
be evadable.
-}

{- | Push an 'EvadeEnemy' for an entity in response to the player using its
'AbilityEvade' UI ability. The entity is identified by an 'EnemyId' for
routing through the evade subsystem (enemy-locations pass a coerced id).
-}
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

{- | Resolve a 'TryEvadeEnemy' message: set up the standard evade skill test
using the entity's stored evade value as the default difficulty.
-}
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

{- | Build the standalone evade-skill-test 'Message' for an entity (without
pushing it).
-}
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

{- | Push the standard successful-evade cascade:
@when SuccessfulEvadeEnemy@, @EnemyEvaded@, @after SuccessfulEvadeEnemy@.

Replacing an evasion hooks the @EnemyWouldBeEvaded@ batch that the enemy runner
wraps around @EnemyEvaded@, not this cascade — that way automatic evasions,
which never reach here, are covered too.
-}
pushSuccessfulEvade
  :: (HasGame m, HasQueue Message m)
  => InvestigatorId -> Source -> EnemyId -> Int -> m ()
pushSuccessfulEvade iid source eid n = do
  whenMsg <- checkWhen $ Window.SuccessfulEvadeEnemy iid source eid n
  afterMsg <- checkAfter $ Window.SuccessfulEvadeEnemy iid source eid n
  pushAll [whenMsg, EnemyEvaded iid eid, afterMsg]

{- | Push the mechanical half of an evasion (@Do msg@) wrapped in the standard
cascade: the @EnemyWouldBeEvaded@ would-batch fronting the when/after
@EnemyEvaded@ windows. Cancelling the batch drops the @Do msg@ along with both
windows, so a reaction that replaces the evasion (Reminiscence (Covenant))
leaves the enemy engaged and ready.
-}
pushEvadedWindows
  :: (HasGame m, HasQueue Message m, MonadRandom m)
  => InvestigatorId -> EnemyId -> Message -> m ()
pushEvadedWindows iid eid msg = do
  (batchId, wouldMsgs) <- wouldWindows $ Window.EnemyWouldBeEvaded iid eid
  conditionTick <- getWindowTick
  whenWindow <- checkWindows [mkWhen $ Window.EnemyEvaded iid eid]
  afterWindow <- checkWindowsAt conditionTick [mkAfter $ Window.EnemyEvaded iid eid]
  push $ Would batchId $ wouldMsgs <> [whenWindow, Do msg, afterWindow]
