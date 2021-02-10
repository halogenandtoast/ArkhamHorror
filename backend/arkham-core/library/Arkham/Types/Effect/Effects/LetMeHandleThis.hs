module Arkham.Types.Effect.Effects.LetMeHandleThis
  ( letMeHandleThis
  , LetMeHandleThis(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype LetMeHandleThis = LetMeHandleThis EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letMeHandleThis :: EffectArgs -> LetMeHandleThis
letMeHandleThis = LetMeHandleThis . uncurry4 (baseAttrs "03022")

instance HasModifiersFor env LetMeHandleThis where
  getModifiersFor source target (LetMeHandleThis a@EffectAttrs {..})
    | source == effectSource && target == effectTarget = pure
      [toModifier a $ AnySkillValue 2]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env LetMeHandleThis where
  runMessage msg e@(LetMeHandleThis attrs) = case msg of
    AfterRevelation _ tid' | effectTarget attrs == TreacheryTarget tid' ->
      e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> LetMeHandleThis <$> runMessage msg attrs
