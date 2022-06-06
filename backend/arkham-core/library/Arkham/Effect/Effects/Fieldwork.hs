module Arkham.Effect.Effects.Fieldwork
  ( Fieldwork(..)
  , fieldwork
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Modifier
import Arkham.Source

newtype Fieldwork = Fieldwork EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fieldwork :: EffectArgs -> Fieldwork
fieldwork = Fieldwork . uncurry4 (baseAttrs "03024")

instance HasModifiersFor Fieldwork where
  getModifiersFor SkillTestSource{} target (Fieldwork a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a (AnySkillValue 2)]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage Fieldwork where
  runMessage msg e@(Fieldwork attrs) = case msg of
    EndPhase -> e <$ push (DisableEffect $ effectId attrs)
    _ -> Fieldwork <$> runMessage msg attrs
