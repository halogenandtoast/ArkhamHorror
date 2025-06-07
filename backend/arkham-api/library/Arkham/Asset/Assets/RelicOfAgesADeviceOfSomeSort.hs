module Arkham.Asset.Assets.RelicOfAgesADeviceOfSomeSort (relicOfAgesADeviceOfSomeSort) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype RelicOfAgesADeviceOfSomeSort = RelicOfAgesADeviceOfSomeSort AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicOfAgesADeviceOfSomeSort :: AssetCard RelicOfAgesADeviceOfSomeSort
relicOfAgesADeviceOfSomeSort = asset RelicOfAgesADeviceOfSomeSort Cards.relicOfAgesADeviceOfSomeSort

instance RunMessage RelicOfAgesADeviceOfSomeSort where
  runMessage msg (RelicOfAgesADeviceOfSomeSort attrs) = RelicOfAgesADeviceOfSomeSort <$> runMessage msg attrs
