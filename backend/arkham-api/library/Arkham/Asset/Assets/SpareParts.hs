module Arkham.Asset.Assets.SpareParts (spareParts) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype SpareParts = SpareParts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spareParts :: AssetCard SpareParts
spareParts = asset SpareParts Cards.spareParts

instance RunMessage SpareParts where
  runMessage msg (SpareParts attrs) = runQueueT $ case msg of
    _ -> SpareParts <$> liftRunMessage msg attrs
