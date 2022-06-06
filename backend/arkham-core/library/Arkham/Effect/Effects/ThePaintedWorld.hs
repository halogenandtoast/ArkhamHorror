module Arkham.Effect.Effects.ThePaintedWorld
  ( ThePaintedWorld(..)
  , thePaintedWorld
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Modifier
import Arkham.Target

newtype ThePaintedWorld = ThePaintedWorld EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EffectArgs -> ThePaintedWorld
thePaintedWorld = ThePaintedWorld . uncurry4 (baseAttrs "03012")

instance HasModifiersFor ThePaintedWorld where
  getModifiersFor _ (EventTarget eid) (ThePaintedWorld a@EffectAttrs {..})
    | CardIdTarget (unEventId eid) == effectTarget = pure
    $ toModifiers a [RemoveFromGameInsteadOfDiscard]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage ThePaintedWorld where
  runMessage msg (ThePaintedWorld attrs) =
    ThePaintedWorld <$> runMessage msg attrs
