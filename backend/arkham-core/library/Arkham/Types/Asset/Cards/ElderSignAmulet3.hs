module Arkham.Types.Asset.Cards.ElderSignAmulet3 where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype ElderSignAmulet3 = ElderSignAmulet3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

elderSignAmulet3 :: AssetCard ElderSignAmulet3
elderSignAmulet3 =
  accessoryWith ElderSignAmulet3 Cards.elderSignAmulet3 (sanityL ?~ 4)

instance HasModifiersFor env ElderSignAmulet3
instance HasActions ElderSignAmulet3

instance (AssetRunner env) => RunMessage env ElderSignAmulet3 where
  runMessage msg (ElderSignAmulet3 attrs) =
    ElderSignAmulet3 <$> runMessage msg attrs
