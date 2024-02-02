module Arkham.Event.Cards.Pilfer3 (
  pilfer3,
  pilfer3Effect,
  Pilfer3 (..),
)
where

import Arkham.Prelude

import Arkham.Aspect
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigate

newtype Pilfer3 = Pilfer3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

pilfer3 :: EventCard Pilfer3
pilfer3 = event Pilfer3 Cards.pilfer3

instance RunMessage Pilfer3 where
  runMessage msg e@(Pilfer3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      investigation <- aspect iid attrs (#agility `InsteadOf` #intellect) (mkInvestigate iid attrs)
      pushAll
        $ skillTestModifier attrs (toTarget iid) (DiscoveredClues 2)
        : leftOr investigation
      pure e
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      push
        $ createCardEffect Cards.pilfer3 (Just $ EffectMetaTarget (toTarget $ toCardId attrs)) attrs iid
      pure e
    _ -> Pilfer3 <$> runMessage msg attrs

newtype Pilfer3Effect = Pilfer3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

pilfer3Effect :: EffectArgs -> Pilfer3Effect
pilfer3Effect = cardEffect Pilfer3Effect Cards.pilfer3

instance RunMessage Pilfer3Effect where
  runMessage msg e@(Pilfer3Effect attrs@EffectAttrs {..}) = case msg of
    EndTurn iid | toTarget iid == effectTarget -> do
      case effectMetadata of
        Just (EffectMetaTarget (CardIdTarget cardId)) -> pushAll [DisableEffect effectId, ReturnToHand iid (toTarget cardId)]
        _ -> error "invalid meta target"
      pure e
    _ -> Pilfer3Effect <$> runMessage msg attrs
