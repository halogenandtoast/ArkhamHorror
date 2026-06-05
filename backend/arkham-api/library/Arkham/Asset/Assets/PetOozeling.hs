module Arkham.Asset.Assets.PetOozeling (petOozeling) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype PetOozeling = PetOozeling AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

petOozeling :: AssetCard PetOozeling
petOozeling = assetWith PetOozeling Cards.petOozeling (healthL ?~ 3)

instance RunMessage PetOozeling where
  runMessage msg (PetOozeling attrs) = PetOozeling <$> runMessage msg attrs
