module Arkham.Asset.Cards.VirgilGray ( virgilGray , VirgilGray(..)) where

import Arkham.Prelude
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype VirgilGray = VirgilGray AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

virgilGray :: AssetCard VirgilGray
virgilGray =
  allyWith VirgilGray Cards.virgilGray (1, 3) (slotsL .~ mempty)

instance RunMessage VirgilGray where
  runMessage msg (VirgilGray attrs) = VirgilGray <$> runMessage msg attrs
