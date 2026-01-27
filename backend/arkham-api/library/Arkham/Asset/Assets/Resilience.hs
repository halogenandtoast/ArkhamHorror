module Arkham.Asset.Assets.Resilience (resilience) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype Resilience = Resilience AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

resilience :: AssetCard Resilience
resilience = assetWith Resilience Cards.resilience ((healthL ?~ 2) . (sanityL ?~ 2))
