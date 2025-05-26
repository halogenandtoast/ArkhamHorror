module Arkham.Effect.Effects.AbilityEffect (AbilityEffect (..), abilityEffect) where

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Prelude

newtype AbilityEffect = AbilityEffect EffectAttrs
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities AbilityEffect where
  getAbilities (AbilityEffect attrs) = case attrs.metadata of
    Just (EffectAbility ability) -> [ability]
    _ -> []

abilityEffect :: EffectArgs -> AbilityEffect
abilityEffect = AbilityEffect . uncurry (baseAttrs "abief")

instance HasModifiersFor AbilityEffect

instance RunMessage AbilityEffect where
  runMessage msg (AbilityEffect attrs) = AbilityEffect <$> runMessage msg attrs
