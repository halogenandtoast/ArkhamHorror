module Arkham.Types.Effect.Effects.CursedShores
  ( cursedShores
  , CursedShores(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype CursedShores = CursedShores Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cursedShores :: EffectArgs -> CursedShores
cursedShores = CursedShores . uncurry4 (baseAttrs "81007")

instance HasModifiersFor env CursedShores where
  getModifiersFor SkillTestSource{} target (CursedShores a@Attrs {..})
    | target == effectTarget = pure [modifier a (AnySkillValue 2)]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env CursedShores where
  runMessage msg e@(CursedShores attrs) = case msg of
    SkillTestEnds -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    EndTurn iid | InvestigatorTarget iid == effectTarget attrs ->
      e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> CursedShores <$> runMessage msg attrs
