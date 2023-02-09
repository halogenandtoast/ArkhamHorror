module Arkham.Event.Cards.MindOverMatter
  ( MindOverMatter
  , mindOverMatter
  , mindOverMatterEffect
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype MindOverMatter = MindOverMatter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter :: EventCard MindOverMatter
mindOverMatter = event MindOverMatter Cards.mindOverMatter

instance RunMessage MindOverMatter where
  runMessage msg e@(MindOverMatter attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      e <$ pushAll
        [ createCardEffect
          Cards.mindOverMatter
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        , discard attrs
        ]
    _ -> MindOverMatter <$> runMessage msg attrs

newtype MindOverMatterEffect = MindOverMatterEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatterEffect :: EffectArgs -> MindOverMatterEffect
mindOverMatterEffect = cardEffect MindOverMatterEffect Cards.mindOverMatter

instance HasModifiersFor MindOverMatterEffect where
  getModifiersFor target (MindOverMatterEffect a@EffectAttrs {..})
    | target == effectTarget = pure $ toModifiers
      a
      [ UseSkillInPlaceOf SkillCombat SkillIntellect
      , UseSkillInPlaceOf SkillAgility SkillIntellect
      ]
  getModifiersFor _ _ = pure []

instance RunMessage MindOverMatterEffect where
  runMessage msg e@(MindOverMatterEffect attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ effectId attrs)
    _ -> MindOverMatterEffect <$> runMessage msg attrs
