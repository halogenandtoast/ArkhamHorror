module Arkham.Effect.Effects.ThePaintedWorld
  ( ThePaintedWorld(..)
  , thePaintedWorld
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Target

newtype ThePaintedWorld = ThePaintedWorld EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EffectArgs -> ThePaintedWorld
thePaintedWorld = ThePaintedWorld . uncurry4 (baseAttrs "03012")

instance HasModifiersFor ThePaintedWorld where
  getModifiersFor (EventTarget eid) (ThePaintedWorld a@EffectAttrs {..})
    | CardIdTarget (unEventId eid) == effectTarget = pure
    $ toModifiers a [RemoveFromGameInsteadOfDiscard]
  getModifiersFor _ _ = pure []

instance RunMessage ThePaintedWorld where
  runMessage msg (ThePaintedWorld attrs) =
    ThePaintedWorld <$> runMessage msg attrs
