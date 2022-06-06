module Arkham.Effect.Effects.LetMeHandleThis
  ( letMeHandleThis
  , LetMeHandleThis(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Modifier
import Arkham.Target

newtype LetMeHandleThis = LetMeHandleThis EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letMeHandleThis :: EffectArgs -> LetMeHandleThis
letMeHandleThis = LetMeHandleThis . uncurry4 (baseAttrs "03022")

instance HasModifiersFor LetMeHandleThis where
  getModifiersFor source target (LetMeHandleThis a@EffectAttrs {..})
    | source == effectSource && target == effectTarget = pure
      [toModifier a $ AnySkillValue 2]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage LetMeHandleThis where
  runMessage msg e@(LetMeHandleThis attrs) = case msg of
    AfterRevelation _ tid' | effectTarget attrs == TreacheryTarget tid' ->
      e <$ push (DisableEffect $ effectId attrs)
    _ -> LetMeHandleThis <$> runMessage msg attrs
