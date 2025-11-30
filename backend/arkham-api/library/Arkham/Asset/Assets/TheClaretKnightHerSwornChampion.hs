module Arkham.Asset.Assets.TheClaretKnightHerSwornChampion (theClaretKnightHerSwornChampion) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheClaretKnightHerSwornChampion = TheClaretKnightHerSwornChampion AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theClaretKnightHerSwornChampion :: AssetCard TheClaretKnightHerSwornChampion
theClaretKnightHerSwornChampion = allyWith TheClaretKnightHerSwornChampion Cards.theClaretKnightHerSwornChampion (4, 2) noSlots

instance RunMessage TheClaretKnightHerSwornChampion where
  runMessage msg (TheClaretKnightHerSwornChampion attrs) = runQueueT $ case msg of
    _ -> TheClaretKnightHerSwornChampion <$> liftRunMessage msg attrs
