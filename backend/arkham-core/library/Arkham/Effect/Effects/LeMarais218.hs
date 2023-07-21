module Arkham.Effect.Effects.LeMarais218 (
  LeMarais218 (..),
  leMarais218,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message

newtype LeMarais218 = LeMarais218 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leMarais218 :: EffectArgs -> LeMarais218
leMarais218 = LeMarais218 . uncurry4 (baseAttrs "03218")

instance HasModifiersFor LeMarais218 where
  getModifiersFor target (LeMarais218 attrs)
    | target == effectTarget attrs =
        pure $ toModifiers attrs [CannotMove]
  getModifiersFor _ _ = pure []

instance RunMessage LeMarais218 where
  runMessage msg e@(LeMarais218 attrs) = case msg of
    EndRoundWindow -> e <$ push (DisableEffect $ toId attrs)
    _ -> LeMarais218 <$> runMessage msg attrs
