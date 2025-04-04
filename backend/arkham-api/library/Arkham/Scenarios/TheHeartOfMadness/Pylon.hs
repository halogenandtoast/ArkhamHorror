module Arkham.Scenarios.TheHeartOfMadness.Pylon where

import Arkham.Action qualified as Action
import Arkham.Calculation
import Arkham.Classes.Entity
import Arkham.Classes.HasQueue
import Arkham.Classes.RunMessage.Internal
import Arkham.Constants
import Arkham.DamageEffect
import Arkham.Fight
import Arkham.Helpers.Enemy (getModifiedDamageAmount, isActionTarget)
import Arkham.Helpers.Message (pushWhen)
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.History
import Arkham.Id
import Arkham.Location.Runner ()
import Arkham.Location.Types
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted hiding (choose)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Window qualified as Window

pylonRunner :: (IsLocation b, Sourceable b, Targetable b) => Runner b
pylonRunner msg pylon = runQueueT $ case msg of
  UseCardAbility iid (isSource pylon -> True) AbilityAttack _ _ -> do
    sid <- getRandom
    push $ FightEnemy (coerce (toAttrs pylon).id) $ mkChooseFightPure sid iid ((toAttrs pylon).ability AbilityAttack)
    pure pylon
  AttackEnemy eid choose | coerce eid == (toAttrs pylon).id -> do
    let iid = choose.investigator
    let source = choose.source
    let sid = choose.skillTest
    let target = maybe (toTarget pylon) (ProxyTarget (toTarget pylon)) choose.target
    let skillType = choose.skillType
    let
      difficulty =
        case choose.difficulty of
          DefaultChooseFightDifficulty -> LocationMaybeFieldCalculation (toAttrs pylon).id LocationShroud
          CalculatedChooseFightDifficulty c -> c

    fight sid iid source target skillType difficulty
    pure pylon
  PassedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isActionTarget pylon target -> do
    updateHistory iid (HistoryItem HistorySuccessfulAttacks 1)
    push $ Successful (Action.Fight, toProxyTarget target) iid source (toActionTarget target) n
    pure pylon
  Successful (Action.Fight, _) iid source target n | isTarget pylon target -> do
    mods <- getModifiers pylon
    let alternateSuccess = [t | AlternateSuccess t <- mods]
    pushWhen (null alternateSuccess) $ InvestigatorDamageEnemy iid (coerce (toAttrs pylon).id) source
    for_ alternateSuccess $ \target' ->
      push $ Successful (Action.Fight, toTarget pylon) iid source target' n
    pure pylon
  Msg.EnemyDamage eid damageAssignment | coerce eid == (toAttrs pylon).id -> do
    let
      source = damageAssignmentSource damageAssignment
      damageEffect = damageAssignmentDamageEffect damageAssignment
      damageAmount = damageAssignmentAmount damageAssignment
    checkWhen $ Window.DealtDamage source damageEffect (toTarget pylon) damageAmount
    checkAfter $ Window.DealtDamage source damageEffect (toTarget pylon) damageAmount
    checkWhen $ Window.TakeDamage source damageEffect (toTarget pylon) damageAmount
    push $ EnemyDamaged eid damageAssignment
    checkAfter $ Window.TakeDamage source damageEffect (toTarget pylon) damageAmount
    pure pylon
  EnemyDamaged eid damageAssignment | coerce eid == (toAttrs pylon).id -> do
    amount' <- getModifiedDamageAmount pylon damageAssignment
    pure $ overAttrs (tokensL %~ addTokens #damage amount') pylon
  _ -> overAttrsM (liftRunMessage msg) pylon
