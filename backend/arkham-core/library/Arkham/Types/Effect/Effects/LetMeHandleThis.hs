module Arkham.Types.Effect.Effects.LetMeHandleThis
  ( letMeHandleThis
  , LetMeHandleThis(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype LetMeHandleThis = LetMeHandleThis Attrs
  deriving newtype (Show, ToJSON, FromJSON)

letMeHandleThis :: EffectArgs -> LetMeHandleThis
letMeHandleThis = LetMeHandleThis . uncurry4 (baseAttrs "03022")

instance HasModifiersFor env LetMeHandleThis where
  getModifiersFor source target (LetMeHandleThis a@Attrs {..})
    | source == effectSource && target == effectTarget = pure
      [modifier a $ AnySkillValue 2]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env LetMeHandleThis where
  runMessage msg e@(LetMeHandleThis attrs) = case msg of
    AfterRevelation _ tid' | effectTarget attrs == TreacheryTarget tid' ->
      e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> LetMeHandleThis <$> runMessage msg attrs
