module Arkham.Effect.Effects.ThePaintedWorld
  ( ThePaintedWorld(..)
  , thePaintedWorld
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Types
import Arkham.Game.Helpers
import Arkham.Projection
import Arkham.Target

newtype ThePaintedWorld = ThePaintedWorld EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EffectArgs -> ThePaintedWorld
thePaintedWorld = ThePaintedWorld . uncurry4 (baseAttrs "03012")

instance HasModifiersFor ThePaintedWorld where
  getModifiersFor (EventTarget eid) (ThePaintedWorld a@EffectAttrs {..}) = do
    card <- field EventCard eid
    pure $ case effectTarget of
      CardTarget c | c == card -> toModifiers a [RemoveFromGameInsteadOfDiscard]
      _ -> []
  getModifiersFor _ _ = pure []

instance RunMessage ThePaintedWorld where
  runMessage msg (ThePaintedWorld attrs) =
    ThePaintedWorld <$> runMessage msg attrs
