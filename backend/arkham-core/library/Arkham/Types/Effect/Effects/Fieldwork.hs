module Arkham.Types.Effect.Effects.Fieldwork
  ( Fieldwork(..)
  , fieldwork
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source

newtype Fieldwork = Fieldwork EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fieldwork :: EffectArgs -> Fieldwork
fieldwork = Fieldwork . uncurry4 (baseAttrs "03024")

instance HasModifiersFor env Fieldwork where
  getModifiersFor SkillTestSource{} target (Fieldwork a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a (AnySkillValue 2)]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Fieldwork where
  runMessage msg e@(Fieldwork attrs) = case msg of
    EndPhase -> e <$ push (DisableEffect $ effectId attrs)
    _ -> Fieldwork <$> runMessage msg attrs
