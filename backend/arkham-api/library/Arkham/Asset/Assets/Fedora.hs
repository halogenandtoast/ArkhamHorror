module Arkham.Asset.Assets.Fedora (fedora) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype Fedora = Fedora AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fedora :: AssetCard Fedora
fedora = assetWith Fedora Cards.fedora ((healthL ?~ 1) . (sanityL ?~ 1))
