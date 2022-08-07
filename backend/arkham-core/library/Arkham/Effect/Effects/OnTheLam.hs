module Arkham.Effect.Effects.OnTheLam
  ( onTheLam
  , OnTheLam(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Effect.Helpers
import Arkham.Message

newtype OnTheLam = OnTheLam EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: EffectArgs -> OnTheLam
onTheLam = OnTheLam . uncurry4 (baseAttrs "01010")

instance HasModifiersFor OnTheLam where
  getModifiersFor target (OnTheLam a@EffectAttrs {..}) =
    pure $ toModifiers a [ CannotBeAttackedByNonElite | target == effectTarget ]

instance RunMessage OnTheLam where
  runMessage msg e@(OnTheLam attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId attrs)
    _ -> OnTheLam <$> runMessage msg attrs
