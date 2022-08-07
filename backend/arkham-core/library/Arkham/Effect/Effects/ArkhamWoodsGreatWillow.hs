module Arkham.Effect.Effects.ArkhamWoodsGreatWillow
  ( ArkhamWoodsGreatWillow(..)
  , arkhamWoodsGreatWillow
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Message

newtype ArkhamWoodsGreatWillow = ArkhamWoodsGreatWillow EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsGreatWillow :: EffectArgs -> ArkhamWoodsGreatWillow
arkhamWoodsGreatWillow = ArkhamWoodsGreatWillow . uncurry4 (baseAttrs "50033")

instance HasModifiersFor ArkhamWoodsGreatWillow where
  getModifiersFor target (ArkhamWoodsGreatWillow attrs)
    | target == effectTarget attrs = pure
    $ toModifiers attrs [AddKeyword Keyword.Surge]
  getModifiersFor _ _ = pure []

instance RunMessage ArkhamWoodsGreatWillow where
  runMessage msg e@(ArkhamWoodsGreatWillow attrs) = case msg of
    Surge _ source | sourceToTarget source == effectTarget attrs ->
      e <$ push (DisableEffect $ toId e)
    _ -> ArkhamWoodsGreatWillow <$> runMessage msg attrs
