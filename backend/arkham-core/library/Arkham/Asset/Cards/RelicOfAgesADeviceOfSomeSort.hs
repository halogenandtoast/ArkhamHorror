module Arkham.Asset.Cards.RelicOfAgesADeviceOfSomeSort (
  relicOfAgesADeviceOfSomeSort,
  RelicOfAgesADeviceOfSomeSort (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype RelicOfAgesADeviceOfSomeSort = RelicOfAgesADeviceOfSomeSort AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

relicOfAgesADeviceOfSomeSort :: AssetCard RelicOfAgesADeviceOfSomeSort
relicOfAgesADeviceOfSomeSort =
  asset RelicOfAgesADeviceOfSomeSort Cards.relicOfAgesADeviceOfSomeSort

instance RunMessage RelicOfAgesADeviceOfSomeSort where
  runMessage msg (RelicOfAgesADeviceOfSomeSort attrs) =
    RelicOfAgesADeviceOfSomeSort <$> runMessage msg attrs
