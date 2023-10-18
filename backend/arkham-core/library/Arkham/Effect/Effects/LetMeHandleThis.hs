module Arkham.Effect.Effects.LetMeHandleThis (
  letMeHandleThis,
  LetMeHandleThis (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Helpers
import Arkham.Effect.Runner

newtype LetMeHandleThis = LetMeHandleThis EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letMeHandleThis :: EffectArgs -> LetMeHandleThis
letMeHandleThis = LetMeHandleThis . uncurry4 (baseAttrs "03022")

instance HasModifiersFor LetMeHandleThis where
  getModifiersFor target (LetMeHandleThis a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a $ AnySkillValue 2]
  getModifiersFor _ _ = pure []

instance RunMessage LetMeHandleThis where
  runMessage msg e@(LetMeHandleThis attrs) = case msg of
    AfterRevelation _ tid' | attrs.target == TreacheryTarget tid' -> do
      push $ disable attrs
      pure e
    _ -> LetMeHandleThis <$> runMessage msg attrs
