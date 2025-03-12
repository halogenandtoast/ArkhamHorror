module Arkham.Asset.Assets.SpiritualHealing4 (spiritualHealing4) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype SpiritualHealing4 = SpiritualHealing4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritualHealing4 :: AssetCard SpiritualHealing4
spiritualHealing4 = asset SpiritualHealing4 Cards.spiritualHealing4

instance RunMessage SpiritualHealing4 where
  runMessage msg (SpiritualHealing4 attrs) = SpiritualHealing4 <$> runMessage msg attrs
