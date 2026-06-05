module Arkham.Asset.Assets.TheMilitarysPlan (theMilitarysPlan) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheMilitarysPlan = TheMilitarysPlan AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMilitarysPlan :: AssetCard TheMilitarysPlan
theMilitarysPlan = asset TheMilitarysPlan Cards.theMilitarysPlan

instance RunMessage TheMilitarysPlan where
  runMessage msg (TheMilitarysPlan attrs) = TheMilitarysPlan <$> runMessage msg attrs
