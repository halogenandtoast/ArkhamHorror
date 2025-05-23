module Arkham.Asset.Assets.RelicHunter3 (relicHunter3) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Slot

newtype RelicHunter3 = RelicHunter3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicHunter3 :: AssetCard RelicHunter3
relicHunter3 = asset RelicHunter3 Cards.relicHunter3

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) []

instance RunMessage RelicHunter3 where
  runMessage msg (RelicHunter3 attrs) = runQueueT $ case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid #accessory (slot attrs)
      RelicHunter3 <$> liftRunMessage msg attrs
    _ -> RelicHunter3 <$> liftRunMessage msg attrs
