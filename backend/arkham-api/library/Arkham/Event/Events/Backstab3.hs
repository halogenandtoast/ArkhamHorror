module Arkham.Event.Events.Backstab3 (backstab3, backstab3Effect, Backstab3 (..)) where

import Arkham.Action
import Arkham.Aspect hiding (aspect)
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestSource)

newtype Backstab3 = Backstab3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab3 :: EventCard Backstab3
backstab3 = event Backstab3 Cards.backstab3

instance HasModifiersFor Backstab3 where
  getModifiersFor (Backstab3 attrs) = maybeModified_ attrs attrs.controller do
    Fight <- MaybeT getSkillTestAction
    guardM $ MaybeT $ isSource attrs <$$> getSkillTestSource
    pure [DamageDealt 2]

instance RunMessage Backstab3 where
  runMessage msg e@(Backstab3 attrs@EventAttrs {..}) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (DamageDealt 2)
      aspect iid attrs (#agility `InsteadOf` #combat) (mkChooseFight sid iid attrs)
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 2 -> do
      createCardEffect Cards.backstab3 (effectMetaTarget attrs.cardId) attrs iid
      pure e
    _ -> Backstab3 <$> liftRunMessage msg attrs

newtype Backstab3Effect = Backstab3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab3Effect :: EffectArgs -> Backstab3Effect
backstab3Effect = cardEffect Backstab3Effect Cards.backstab3

instance RunMessage Backstab3Effect where
  runMessage msg e@(Backstab3Effect attrs) = runQueueT $ case msg of
    When (EndTurn iid) | toTarget iid == attrs.target -> do
      case attrs.meta of
        Just (EffectMetaTarget target) -> do
          disable attrs
          push $ ReturnToHand iid target
        _ -> error "invalid meta target"
      pure e
    _ -> Backstab3Effect <$> liftRunMessage msg attrs
