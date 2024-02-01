module Arkham.Asset.Cards.ArcaneResearch (
  arcaneResearch,
  ArcaneResearch (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype ArcaneResearch = ArcaneResearch AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

arcaneResearch :: AssetCard ArcaneResearch
arcaneResearch = asset ArcaneResearch Cards.arcaneResearch

instance RunMessage ArcaneResearch where
  runMessage msg (ArcaneResearch attrs) =
    ArcaneResearch <$> runMessage msg attrs
