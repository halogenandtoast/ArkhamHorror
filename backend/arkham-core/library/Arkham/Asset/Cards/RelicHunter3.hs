module Arkham.Asset.Cards.RelicHunter3
  ( relicHunter3
  , RelicHunter3(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype RelicHunter3 = RelicHunter3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicHunter3 :: AssetCard RelicHunter3
relicHunter3 = asset RelicHunter3 Cards.relicHunter3

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) Nothing

instance RunMessage RelicHunter3 where
  runMessage msg (RelicHunter3 attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      push $ AddSlot iid AccessorySlot (slot attrs)
      RelicHunter3 <$> runMessage msg attrs
    _ -> RelicHunter3 <$> runMessage msg attrs
