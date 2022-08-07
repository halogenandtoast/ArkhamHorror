module Arkham.Asset.Cards.LeatherCoat where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype LeatherCoat = LeatherCoat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leatherCoat :: AssetCard LeatherCoat
leatherCoat = assetWith LeatherCoat Cards.leatherCoat (healthL ?~ 2)

instance RunMessage LeatherCoat where
  runMessage msg (LeatherCoat attrs) = LeatherCoat <$> runMessage msg attrs
