module Arkham.Event.Cards.BindMonster2 (bindMonster2, bindMonster2Effect, BindMonster2 (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Classes.HasQueue (withQueue_)
import Arkham.Effect.Import
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards (bindMonster2)
import Arkham.Event.Import.Lifted
import Arkham.Exception
import Arkham.Matcher
import Arkham.Placement

newtype BindMonster2 = BindMonster2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2 :: EventCard BindMonster2
bindMonster2 = event BindMonster2 Cards.bindMonster2

instance HasAbilities BindMonster2 where
  getAbilities (BindMonster2 x) = case x.attachedTo of
    Just (EnemyTarget eid) -> [restricted x 1 ControlsThis $ freeReaction (EnemyWouldReady #when $ EnemyWithId eid)]
    _ -> []

instance RunMessage BindMonster2 where
  runMessage msg e@(BindMonster2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      createCardEffect Cards.bindMonster2 Nothing attrs SkillTestTarget
      pushAllM $ leftOr <$> aspect iid attrs (#willpower `InsteadOf` #agility) (mkChooseEvade iid attrs)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      case attrs.attachedTo of
        Just target -> beginSkillTest iid (attrs.ability 1) target #willpower (Fixed 3)
        Nothing -> throwIO $ InvalidState "must be attached"
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      case attrs.attachedTo of
        Just target@(EnemyTarget _) -> lift $ withQueue_ (filter (/= Ready target))
        _ -> error "invalid target"
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> BindMonster2 <$> liftRunMessage msg attrs

newtype BindMonster2Effect = BindMonster2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2Effect :: EffectArgs -> BindMonster2Effect
bindMonster2Effect = cardEffect BindMonster2Effect Cards.bindMonster2

instance RunMessage BindMonster2Effect where
  runMessage msg e@(BindMonster2Effect attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Action.Evade) _ (Initiator (EnemyTarget eid)) _ _ -> do
      case attrs.source of
        EventSource evid -> whenM (eid <=~> NonEliteEnemy) do
          push $ PlaceEvent iid evid (AttachedToEnemy eid)
          disable attrs
        _ -> pure ()
      pure e
    SkillTestEnds _ _ -> disableReturn e
    _ -> BindMonster2Effect <$> liftRunMessage msg attrs
