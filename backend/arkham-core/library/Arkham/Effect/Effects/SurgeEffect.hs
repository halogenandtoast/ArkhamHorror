module Arkham.Effect.Effects.SurgeEffect (
  SurgeEffect (..),
  surgeEffect,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword

newtype SurgeEffect = SurgeEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

surgeEffect :: EffectArgs -> SurgeEffect
surgeEffect = SurgeEffect . uncurry (baseAttrs "surge")

instance HasModifiersFor SurgeEffect where
  getModifiersFor target (SurgeEffect EffectAttrs {..}) | target == effectTarget = do
    pure $ toModifiers effectSource [AddKeyword Keyword.Surge]
  getModifiersFor _ _ = pure []

instance RunMessage SurgeEffect where
  runMessage msg e@(SurgeEffect attrs) = case msg of
    ResolvedCard _ card | toTarget (toCardId card) == effectTarget attrs -> do
      push $ DisableEffect (toId attrs)
      pure e
    _ -> SurgeEffect <$> runMessage msg attrs
