module Arkham.Asset.Cards.VirgilGrayTrulyInspired (
  virgilGrayTrulyInspired,
  VirgilGrayTrulyInspired (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype VirgilGrayTrulyInspired = VirgilGrayTrulyInspired AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

virgilGrayTrulyInspired :: AssetCard VirgilGrayTrulyInspired
virgilGrayTrulyInspired =
  asset VirgilGrayTrulyInspired Cards.virgilGrayTrulyInspired

instance RunMessage VirgilGrayTrulyInspired where
  runMessage msg (VirgilGrayTrulyInspired attrs) = VirgilGrayTrulyInspired <$> runMessage msg attrs
