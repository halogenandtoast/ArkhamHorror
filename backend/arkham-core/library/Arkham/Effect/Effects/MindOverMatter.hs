module Arkham.Effect.Effects.MindOverMatter
  ( mindOverMatter
  , MindOverMatter(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType

newtype MindOverMatter = MindOverMatter EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter :: EffectArgs -> MindOverMatter
mindOverMatter = MindOverMatter . uncurry4 (baseAttrs "01036")

instance HasModifiersFor MindOverMatter where
  getModifiersFor _ target (MindOverMatter a@EffectAttrs {..})
    | target == effectTarget = pure $ toModifiers
      a
      [ UseSkillInPlaceOf SkillCombat SkillIntellect
      , UseSkillInPlaceOf SkillAgility SkillIntellect
      ]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage MindOverMatter where
  runMessage msg e@(MindOverMatter attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ effectId attrs)
    _ -> MindOverMatter <$> runMessage msg attrs
