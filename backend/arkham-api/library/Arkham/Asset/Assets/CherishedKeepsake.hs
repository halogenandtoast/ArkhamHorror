module Arkham.Asset.Assets.CherishedKeepsake where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype CherishedKeepsake = CherishedKeepsake AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cherishedKeepsake :: AssetCard CherishedKeepsake
cherishedKeepsake = assetWith CherishedKeepsake Cards.cherishedKeepsake (sanityL ?~ 2)

instance RunMessage CherishedKeepsake where
  runMessage msg (CherishedKeepsake attrs) = CherishedKeepsake <$> runMessage msg attrs
