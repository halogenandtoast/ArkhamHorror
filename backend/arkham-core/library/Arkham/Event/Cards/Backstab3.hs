module Arkham.Event.Cards.Backstab3 (backstab3, backstab3Effect, Backstab3 (..)) where

import Arkham.Action
import Arkham.Aspect
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Modifiers (toModifier)
import Arkham.Helpers.SkillTest
import Arkham.Modifier

newtype Backstab3 = Backstab3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab3 :: EventCard Backstab3
backstab3 = event Backstab3 Cards.backstab3

instance HasModifiersFor Backstab3 where
  getModifiersFor (InvestigatorTarget _) (Backstab3 attrs) =
    maybeToList <$> runMaybeT do
      Fight <- MaybeT getSkillTestAction
      guardM $ MaybeT $ isSource attrs <$$> getSkillTestSource
      pure $ toModifier attrs $ DamageDealt 2
  getModifiersFor _ _ = pure []

instance RunMessage Backstab3 where
  runMessage msg e@(Backstab3 attrs@EventAttrs {..}) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      skillTestModifier attrs iid (DamageDealt 2)
      pushAllM $ leftOr <$> aspect iid attrs (#agility `InsteadOf` #combat) (mkChooseFight iid attrs)
      pure e
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      createCardEffect Cards.pilfer3 (effectMetaTarget attrs.cardId) attrs iid
      pure e
    _ -> Backstab3 <$> lift (runMessage msg attrs)

newtype Backstab3Effect = Backstab3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab3Effect :: EffectArgs -> Backstab3Effect
backstab3Effect = cardEffect Backstab3Effect Cards.backstab3

instance RunMessage Backstab3Effect where
  runMessage msg e@(Backstab3Effect attrs) = runQueueT $ case msg of
    EndTurn iid | toTarget iid == attrs.target -> do
      case attrs.meta of
        Just (EffectMetaTarget target) -> do
          disable attrs
          push $ ReturnToHand iid target
        _ -> error "invalid meta target"
      pure e
    _ -> Backstab3Effect <$> lift (runMessage msg attrs)
