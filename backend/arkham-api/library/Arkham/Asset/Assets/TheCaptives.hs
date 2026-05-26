module Arkham.Asset.Assets.TheCaptives (theCaptives) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheCaptives = TheCaptives AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCaptives :: AssetCard TheCaptives
theCaptives = assetWith TheCaptives Cards.theCaptives (healthL ?~ 10)
