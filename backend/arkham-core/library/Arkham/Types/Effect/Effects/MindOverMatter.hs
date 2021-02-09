module Arkham.Types.Effect.Effects.MindOverMatter
  ( mindOverMatter
  , MindOverMatter(..)
  )
where


import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype MindOverMatter = MindOverMatter EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter :: EffectArgs -> MindOverMatter
mindOverMatter = MindOverMatter . uncurry4 (baseAttrs "01036")

instance HasModifiersFor env MindOverMatter where
  getModifiersFor _ target (MindOverMatter a@EffectAttrs {..})
    | target == effectTarget = pure $ toModifiers
      a
      [ UseSkillInPlaceOf SkillCombat SkillIntellect
      , UseSkillInPlaceOf SkillAgility SkillIntellect
      ]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env MindOverMatter where
  runMessage msg e@(MindOverMatter attrs) = case msg of
    EndRound -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> MindOverMatter <$> runMessage msg attrs
