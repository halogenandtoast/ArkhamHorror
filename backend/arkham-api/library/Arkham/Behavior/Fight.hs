module Arkham.Behavior.Fight where

import Arkham.Action qualified as Action
import Arkham.Calculation
import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasQueue (HasQueue)
import Arkham.Fight (mkChooseFightPure)
import Arkham.Fight.Types
import Arkham.Helpers.Message (push, pushAll)
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.SkillTest.Lifted (fight)
import Arkham.Helpers.Window (checkAfter, checkWhen)
import Arkham.Id
import Arkham.Message
  ( Message
  , pattern FightTarget
  , pattern InvestigatorDamageEnemy
  , pattern Successful
  )
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Window qualified as Window

{- | The 'Fightable' behavior. An entity that is fightable has a fight value
(or a calculated difficulty) and responds to 'AttackTarget' messages by
spinning up a fight skill test.

Used by both 'Arkham.Enemy.Runner' (real enemies) and
'Arkham.EnemyLocation.Runner' (enemy-locations) — and by anything else we want
to make fightable in the future.
-}

-- | Push a 'FightTarget' for an entity in response to a player using its
-- 'AbilityAttack' UI ability.
pushAttackAbility
  :: (HasQueue Message m, MonadRandom m, Sourceable s, Targetable t)
  => t -> InvestigatorId -> s -> m ()
pushAttackAbility entity iid src = do
  sid <- getRandom
  push $ FightTarget (toTarget entity) $ mkChooseFightPure sid iid (toSource src)

-- | Resolve an 'AttackTarget' message: set up the standard fight skill test
-- using the entity's stored fight value as the default difficulty. The caller
-- is responsible for matching 'AttackTarget tgt choose' against its own
-- target; this helper just runs the setup.
resolveAttack
  :: (ReverseQueue m, Targetable a)
  => a
  -- ^ the entity (used as the resolution target if @choose.target@ is unset)
  -> Maybe GameCalculation
  -- ^ the entity's fight value, if any; @Nothing@ falls back to 'Fixed 0'
  -> ChooseFight
  -> m ()
resolveAttack entity defaultDifficulty choose = do
  let entityTarget = toTarget entity
  let target = maybe entityTarget (ProxyTarget entityTarget) choose.target
  let difficulty = case choose.difficulty of
        DefaultChooseFightDifficulty -> fromMaybe (Fixed 0) defaultDifficulty
        CalculatedChooseFightDifficulty c -> c
  fight
    choose.skillTest
    choose.investigator
    choose.source
    target
    choose.skillType
    difficulty

-- | Build the standalone fight-skill-test 'Message' for an entity (without
-- pushing it). Useful when the caller needs to position the fight inside its
-- own window cascade — e.g. 'Arkham.Enemy.Runner' wraps the fight in
-- 'Window.EnemyAttacked' / 'Window.AttemptToFightEnemy' windows.
mkAttackMessage
  :: Targetable a
  => a -> GameCalculation -> ChooseFight -> Message
mkAttackMessage entity defaultDifficulty choose =
  let entityTarget = toTarget entity
      target = maybe entityTarget (ProxyTarget entityTarget) choose.target
      difficulty = case choose.difficulty of
        DefaultChooseFightDifficulty -> defaultDifficulty
        CalculatedChooseFightDifficulty c -> c
   in Msg.fight
        choose.skillTest
        choose.investigator
        choose.source
        target
        choose.skillType
        difficulty

-- | Push the standard successful-attack cascade for an entity:
-- @when SuccessfulAttackEnemy@, damage, @Successful (Fight, _)@,
-- @after SuccessfulAttackEnemy@. The entity is identified by an 'EnemyId' for
-- routing through the fight/damage subsystem (enemy-locations pass a coerced id).
pushSuccessfulAttack
  :: (HasGame m, HasQueue Message m)
  => InvestigatorId -> Source -> EnemyId -> Int -> m ()
pushSuccessfulAttack iid source eid n = do
  whenMsg <- checkWhen $ Window.SuccessfulAttackEnemy iid source eid n
  afterMsg <- checkAfter $ Window.SuccessfulAttackEnemy iid source eid n
  pushAll
    [ whenMsg
    , InvestigatorDamageEnemy iid eid source
    , Successful (Action.Fight, EnemyTarget eid) iid source (EnemyTarget eid) n
    , afterMsg
    ]
