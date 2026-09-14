module Arkham.Asset.Assets.ArcaneResearch (arcaneResearch) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype ArcaneResearch = ArcaneResearch AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneResearch :: AssetCard ArcaneResearch
arcaneResearch = asset ArcaneResearch Cards.arcaneResearch
