module Arkham.Types.Effect.Effects.Lucky
  ( lucky
  , Lucky(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype Lucky = Lucky Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lucky :: EffectArgs -> Lucky
lucky = Lucky . uncurry4 (baseAttrs "01080")

instance HasModifiersFor env Lucky where
  getModifiersFor _ target (Lucky Attrs {..}) | target == effectTarget =
    pure [AnySkillValue 2]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Lucky where
  runMessage msg e@(Lucky attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget _) | eid == effectId attrs ->
      e <$ unshiftMessage RerunSkillTest
    SkillTestEnds -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> Lucky <$> runMessage msg attrs
