module Arkham.Types.Effect.Effects.DaisysToteBagAdvanced
  ( daisysToteBagAdvanced
  , DaisysToteBagAdvanced(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype DaisysToteBagAdvanced = DaisysToteBagAdvanced EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBagAdvanced :: EffectArgs -> DaisysToteBagAdvanced
daisysToteBagAdvanced = DaisysToteBagAdvanced . uncurry4 (baseAttrs "90002")

instance HasModifiersFor env DaisysToteBagAdvanced where
  getModifiersFor _ target (DaisysToteBagAdvanced attrs@EffectAttrs {..})
    | target == effectTarget = pure (toModifiers attrs [BecomesFast])
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env DaisysToteBagAdvanced where
  runMessage msg e@(DaisysToteBagAdvanced attrs) = case msg of
    PlayedCard _ card | CardIdTarget (toCardId card) == effectTarget attrs ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> DaisysToteBagAdvanced <$> runMessage msg attrs
