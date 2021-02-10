module Arkham.Types.Effect.Effects.HuntingNightgaunt
  ( huntingNightgaunt
  , HuntingNightgaunt(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Action
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype HuntingNightgaunt = HuntingNightgaunt EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingNightgaunt :: EffectArgs -> HuntingNightgaunt
huntingNightgaunt = HuntingNightgaunt . uncurry4 (baseAttrs "01172")

instance HasModifiersFor env HuntingNightgaunt where
  getModifiersFor (SkillTestSource _ _ _ (Just Evade)) (DrawnTokenTarget _) (HuntingNightgaunt a@EffectAttrs {..})
    = pure $ toModifiers a [DoubleNegativeModifiersOnTokens]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env HuntingNightgaunt where
  runMessage msg e@(HuntingNightgaunt attrs) = case msg of
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> HuntingNightgaunt <$> runMessage msg attrs
