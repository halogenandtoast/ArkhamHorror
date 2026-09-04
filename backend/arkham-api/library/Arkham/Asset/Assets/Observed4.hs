module Arkham.Asset.Assets.Observed4 (observed4) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype Observed4 = Observed4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

observed4 :: AssetCard Observed4
observed4 = asset Observed4 Cards.observed4
