module Arkham.Types.Effect.Effects.Deduction
  ( deduction
  , Deduction(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype Deduction = Deduction Attrs
  deriving newtype (Show, ToJSON, FromJSON)

deduction :: EffectArgs -> Deduction
deduction = Deduction . uncurry4 (baseAttrs "01039")

instance HasModifiersFor env Deduction where
  getModifiersFor _source target (Deduction a@Attrs {..})
    | target == effectTarget = pure [modifier a (DiscoveredClues 1)]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Deduction where
  runMessage msg e@(Deduction attrs) = case msg of
    SkillTestEnds -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> Deduction <$> runMessage msg attrs
