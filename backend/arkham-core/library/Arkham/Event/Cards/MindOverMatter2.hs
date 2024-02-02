module Arkham.Event.Cards.MindOverMatter2 (
  MindOverMatter2,
  mindOverMatter2,
  mindOverMatter2Effect,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.SkillType

newtype MindOverMatter2 = MindOverMatter2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

mindOverMatter2 :: EventCard MindOverMatter2
mindOverMatter2 = event MindOverMatter2 Cards.mindOverMatter2

instance RunMessage MindOverMatter2 where
  runMessage msg e@(MindOverMatter2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      drawing <- drawCards iid attrs 1
      pushAll
        [ createCardEffect
            Cards.mindOverMatter2
            Nothing
            (toSource attrs)
            (InvestigatorTarget iid)
        , drawing
        ]
      pure e
    _ -> MindOverMatter2 <$> runMessage msg attrs

newtype MindOverMatter2Effect = MindOverMatter2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

mindOverMatter2Effect :: EffectArgs -> MindOverMatter2Effect
mindOverMatter2Effect = cardEffect MindOverMatter2Effect Cards.mindOverMatter2

instance HasModifiersFor MindOverMatter2Effect where
  getModifiersFor target (MindOverMatter2Effect a@EffectAttrs {..})
    | target == effectTarget =
        pure
          $ toModifiers
            a
            [ AddSkillToOtherSkill SkillIntellect SkillCombat
            , AddSkillToOtherSkill SkillIntellect SkillAgility
            ]
  getModifiersFor _ _ = pure []

instance RunMessage MindOverMatter2Effect where
  runMessage msg e@(MindOverMatter2Effect attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ effectId attrs)
    _ -> MindOverMatter2Effect <$> runMessage msg attrs
