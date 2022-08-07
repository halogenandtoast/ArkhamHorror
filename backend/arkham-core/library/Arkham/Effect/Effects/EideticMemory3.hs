module Arkham.Effect.Effects.EideticMemory3
  ( EideticMemory3(..)
  , eideticMemory3
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Target

newtype EideticMemory3 = EideticMemory3 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eideticMemory3 :: EffectArgs -> EideticMemory3
eideticMemory3 = EideticMemory3 . uncurry4 (baseAttrs "03306")

instance HasModifiersFor EideticMemory3 where
  getModifiersFor (EventTarget eid) (EideticMemory3 a@EffectAttrs {..})
    | CardIdTarget (unEventId eid) == effectTarget = pure
    $ toModifiers a [RemoveFromGameInsteadOfDiscard]
  getModifiersFor _ _ = pure []

instance RunMessage EideticMemory3 where
  runMessage msg (EideticMemory3 attrs) =
    EideticMemory3 <$> runMessage msg attrs
