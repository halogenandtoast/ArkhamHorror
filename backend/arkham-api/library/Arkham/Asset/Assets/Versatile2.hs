module Arkham.Asset.Assets.Versatile2 (versatile2) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype Versatile2 = Versatile2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

versatile2 :: AssetCard Versatile2
versatile2 = asset Versatile2 Cards.versatile2
