module Arkham.Types.Asset.Cards.RelicHunter3
  ( relicHunter3
  , RelicHunter3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot

newtype RelicHunter3 = RelicHunter3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicHunter3 :: AssetCard RelicHunter3
relicHunter3 = asset RelicHunter3 Cards.relicHunter3

instance HasActions RelicHunter3
instance HasModifiersFor env RelicHunter3

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) Nothing

instance HasModifiersFor env () => RunMessage env RelicHunter3 where
  runMessage msg (RelicHunter3 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      push $ AddSlot iid AccessorySlot (slot attrs)
      RelicHunter3 <$> runMessage msg attrs
    _ -> RelicHunter3 <$> runMessage msg attrs
