module Arkham.Event.Cards.MindOverMatter (
  MindOverMatter,
  mindOverMatter,
  mindOverMatterEffect,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Message

newtype MindOverMatter = MindOverMatter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter :: EventCard MindOverMatter
mindOverMatter = event MindOverMatter Cards.mindOverMatter

instance RunMessage MindOverMatter where
  runMessage msg e@(MindOverMatter attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ createCardEffect Cards.mindOverMatter Nothing attrs iid
      pure e
    _ -> MindOverMatter <$> runMessage msg attrs

newtype MindOverMatterEffect = MindOverMatterEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatterEffect :: EffectArgs -> MindOverMatterEffect
mindOverMatterEffect = cardEffect MindOverMatterEffect Cards.mindOverMatter

instance HasModifiersFor MindOverMatterEffect where
  getModifiersFor target (MindOverMatterEffect a) | target == effectTarget a = do
    pure
      $ toModifiers a [UseSkillInPlaceOf #combat #intellect, UseSkillInPlaceOf #agility #intellect]
  getModifiersFor _ _ = pure []

instance RunMessage MindOverMatterEffect where
  runMessage msg e@(MindOverMatterEffect attrs) = case msg of
    EndRound -> do
      push $ DisableEffect $ effectId attrs
      pure e
    _ -> MindOverMatterEffect <$> runMessage msg attrs
