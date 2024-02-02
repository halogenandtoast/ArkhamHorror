module Arkham.Asset.Cards.ElderSignAmulet3 where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype ElderSignAmulet3 = ElderSignAmulet3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

elderSignAmulet3 :: AssetCard ElderSignAmulet3
elderSignAmulet3 = assetWith ElderSignAmulet3 Cards.elderSignAmulet3 (sanityL ?~ 4)

instance RunMessage ElderSignAmulet3 where
  runMessage msg (ElderSignAmulet3 attrs) = ElderSignAmulet3 <$> runMessage msg attrs
