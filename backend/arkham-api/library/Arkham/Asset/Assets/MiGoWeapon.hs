module Arkham.Asset.Assets.MiGoWeapon (miGoWeapon) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype MiGoWeapon = MiGoWeapon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoWeapon :: AssetCard MiGoWeapon
miGoWeapon = asset MiGoWeapon Cards.miGoWeapon

instance RunMessage MiGoWeapon where
  runMessage msg (MiGoWeapon attrs) = MiGoWeapon <$> runMessage msg attrs
