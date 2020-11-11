module Arkham.Types.Effect.Effects.LetMeHandleThis
  ( letMeHandleThis
  , LetMeHandleThis(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype LetMeHandleThis = LetMeHandleThis Attrs
  deriving newtype (Show, ToJSON, FromJSON)

letMeHandleThis :: Source -> Target -> EffectId -> LetMeHandleThis
letMeHandleThis source target eid =
  LetMeHandleThis $ baseAttrs eid "03022" source target

instance HasModifiersFor env LetMeHandleThis where
  getModifiersFor source target (LetMeHandleThis Attrs {..})
    | source == effectSource && target == effectTarget = pure [AnySkillValue 2]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env LetMeHandleThis where
  runMessage msg e@(LetMeHandleThis attrs) = case msg of
    AfterRevelation _ tid' | effectTarget attrs == TreacheryTarget tid' ->
      e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> LetMeHandleThis <$> runMessage msg attrs
