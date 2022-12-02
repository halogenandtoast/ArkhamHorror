module Arkham.Effect.Effects.Fieldwork
  ( Fieldwork(..)
  , fieldwork
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Message

newtype Fieldwork = Fieldwork EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fieldwork :: EffectArgs -> Fieldwork
fieldwork = Fieldwork . uncurry4 (baseAttrs "03024")

instance HasModifiersFor Fieldwork where
  getModifiersFor target (Fieldwork a@EffectAttrs {..})
    | target == effectTarget = do
      mSkillTestSource <- getSkillTestSource
      pure [ toModifier a (AnySkillValue 2) | isJust mSkillTestSource ]
  getModifiersFor _ _ = pure []

instance RunMessage Fieldwork where
  runMessage msg e@(Fieldwork attrs) = case msg of
    EndPhase -> e <$ push (DisableEffect $ effectId attrs)
    SkillTestEnds _ _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> Fieldwork <$> runMessage msg attrs
