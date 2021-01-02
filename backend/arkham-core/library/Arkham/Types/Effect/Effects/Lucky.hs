module Arkham.Types.Effect.Effects.Lucky
  ( lucky
  , Lucky(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype Lucky = Lucky Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lucky :: EffectArgs -> Lucky
lucky = Lucky . uncurry4 (baseAttrs "01080")

instance HasModifiersFor env Lucky where
  getModifiersFor _ target (Lucky a@Attrs {..}) | target == effectTarget =
    pure [toModifier a $ AnySkillValue 2]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Lucky where
  runMessage msg e@(Lucky attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget _) | eid == effectId attrs ->
      e <$ unshiftMessage RerunSkillTest
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> Lucky <$> runMessage msg attrs
