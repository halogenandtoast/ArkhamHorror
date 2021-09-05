module Arkham.Types.Effect.Effects.ArkhamWoodsGreatWillow
  ( ArkhamWoodsGreatWillow(..)
  , arkhamWoodsGreatWillow
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Message
import Arkham.Types.Modifier

newtype ArkhamWoodsGreatWillow = ArkhamWoodsGreatWillow EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsGreatWillow :: EffectArgs -> ArkhamWoodsGreatWillow
arkhamWoodsGreatWillow = ArkhamWoodsGreatWillow . uncurry4 (baseAttrs "50033")

instance HasModifiersFor env ArkhamWoodsGreatWillow where
  getModifiersFor _ target (ArkhamWoodsGreatWillow attrs)
    | target == effectTarget attrs = pure
    $ toModifiers attrs [AddKeyword Keyword.Surge]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env ArkhamWoodsGreatWillow where
  runMessage msg e@(ArkhamWoodsGreatWillow attrs) = case msg of
    Surge _ source | sourceToTarget source == effectTarget attrs ->
      e <$ push (DisableEffect $ toId e)
    _ -> ArkhamWoodsGreatWillow <$> runMessage msg attrs
