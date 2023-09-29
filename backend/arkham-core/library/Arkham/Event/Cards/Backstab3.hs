module Arkham.Event.Cards.Backstab3 (
  backstab3,
  backstab3Effect,
  Backstab3 (..),
)
where

import Arkham.Prelude

import Arkham.Action
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.SkillType

newtype Backstab3 = Backstab3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab3 :: EventCard Backstab3
backstab3 = event Backstab3 Cards.backstab3

instance HasModifiersFor Backstab3 where
  getModifiersFor (InvestigatorTarget _) (Backstab3 attrs) = do
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    case (mAction, mSource) of
      (Just Fight, Just source) | isSource attrs source -> do
        pure $ toModifiers attrs [DamageDealt 2]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Backstab3 where
  runMessage msg e@(Backstab3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      push $ ChooseFightEnemy iid (EventSource eid) Nothing SkillAgility mempty False
      pure e
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n | n >= 2 -> do
      push
        $ createCardEffect Cards.pilfer3 (Just $ EffectMetaTarget (toTarget $ toCardId attrs)) attrs iid
      pure e
    _ -> Backstab3 <$> runMessage msg attrs

newtype Backstab3Effect = Backstab3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab3Effect :: EffectArgs -> Backstab3Effect
backstab3Effect = cardEffect Backstab3Effect Cards.backstab3

instance RunMessage Backstab3Effect where
  runMessage msg e@(Backstab3Effect attrs@EffectAttrs {..}) = case msg of
    EndTurn iid | toTarget iid == effectTarget -> do
      case effectMetadata of
        Just (EffectMetaTarget (CardIdTarget cardId)) -> pushAll [DisableEffect effectId, ReturnToHand iid (toTarget cardId)]
        _ -> error "invalid meta target"
      pure e
    _ -> Backstab3Effect <$> runMessage msg attrs
