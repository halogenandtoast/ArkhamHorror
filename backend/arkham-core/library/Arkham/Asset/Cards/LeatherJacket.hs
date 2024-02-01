module Arkham.Asset.Cards.LeatherJacket (
  leatherJacket,
  LeatherJacket (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype LeatherJacket = LeatherJacket AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

leatherJacket :: AssetCard LeatherJacket
leatherJacket =
  assetWith LeatherJacket Cards.leatherJacket (healthL ?~ 2)

instance RunMessage LeatherJacket where
  runMessage msg (LeatherJacket attrs) = LeatherJacket <$> runMessage msg attrs
