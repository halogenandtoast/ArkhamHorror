module Arkham.Asset.Assets.LeoDeLuca1 where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers

newtype LeoDeLuca1 = LeoDeLuca1 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoDeLuca1 :: AssetCard LeoDeLuca1
leoDeLuca1 = ally LeoDeLuca1 Cards.leoDeLuca1 (2, 2)

instance HasModifiersFor LeoDeLuca1 where
  getModifiersFor (LeoDeLuca1 a) = controllerGets a [AdditionalActions "Leo de Luca" (toSource a) 1]

instance RunMessage LeoDeLuca1 where
  runMessage msg (LeoDeLuca1 attrs) = LeoDeLuca1 <$> runMessage msg attrs
