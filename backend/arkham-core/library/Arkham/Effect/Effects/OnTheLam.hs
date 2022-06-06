module Arkham.Effect.Effects.OnTheLam
  ( onTheLam
  , OnTheLam(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Modifier

newtype OnTheLam = OnTheLam EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: EffectArgs -> OnTheLam
onTheLam = OnTheLam . uncurry4 (baseAttrs "01010")

instance HasModifiersFor OnTheLam where
  getModifiersFor _ target (OnTheLam a@EffectAttrs {..}) =
    pure $ toModifiers a [ CannotBeAttackedByNonElite | target == effectTarget ]

instance HasQueue env => RunMessage OnTheLam where
  runMessage msg e@(OnTheLam attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId attrs)
    _ -> OnTheLam <$> runMessage msg attrs
