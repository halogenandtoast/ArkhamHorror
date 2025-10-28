module Arkham.Scenarios.TheHeartOfMadness.Pylon where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Calculation
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Classes.Entity
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
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Import.Lifted hiding (choose)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Scenarios.TheHeartOfMadness.Helpers
import Arkham.Token
import Arkham.Window qualified as Window

pylonAbilities
  :: (Entity pylon, EntityAttrs pylon ~ LocationAttrs) => SealKind -> pylon -> [Ability]
pylonAbilities skind pylon =
  let a = toAttrs pylon
   in extendRevealed
        a
        [ basicAbility $ restricted a AbilityAttack Here $ ActionAbility [#fight] (ActionCost 1)
        , restricted a 1 (Here <> youExist (InvestigatorWithActiveSeal skind)) $ FastAbility Free
        ]

pylonRunner :: (IsLocation b, Sourceable b, Targetable b) => SealKind -> Runner b
pylonRunner skind msg pylon = runQueueT $ case msg of
  UseThisAbility iid (isSource pylon -> True) 1 -> do
    mseal <- find (\s -> s.kind == skind) . toList <$> field InvestigatorSeals iid
    for_ mseal (placeSeal (toAttrs pylon))
    pure pylon
  UseCardAbility iid (isSource pylon -> True) AbilityAttack _ _ -> do
    sid <- getRandom
    push
      $ FightEnemy (coerce (toAttrs pylon).id)
      $ mkChooseFightPure sid iid ((toAttrs pylon).ability AbilityAttack)
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
    mods <- getModifiers pylon
    unless (CannotBeDamaged `elem` mods) do
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
    mods <- getModifiers pylon
    if CannotBeDamaged `elem` mods
      then pure pylon
      else do
        amount' <- getModifiedDamageAmount pylon damageAssignment
        pure $ overAttrs (tokensL %~ addTokens #damage amount') pylon
  _ -> overAttrsM (liftRunMessage msg) pylon
