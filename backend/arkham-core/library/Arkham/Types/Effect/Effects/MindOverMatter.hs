module Arkham.Types.Effect.Effects.MindOverMatter
  ( mindOverMatter
  , MindOverMatter(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype MindOverMatter = MindOverMatter Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mindOverMatter :: EffectArgs -> MindOverMatter
mindOverMatter = MindOverMatter . uncurry4 (baseAttrs "01036")

instance HasModifiersFor env MindOverMatter where
  getModifiersFor _ target (MindOverMatter Attrs {..})
    | target == effectTarget = pure
      [ UseSkillInPlaceOf SkillCombat SkillIntellect
      , UseSkillInPlaceOf SkillAgility SkillIntellect
      ]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env MindOverMatter where
  runMessage msg e@(MindOverMatter attrs) = case msg of
    EndRound -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> MindOverMatter <$> runMessage msg attrs
