module Arkham.Asset.Assets.MerleGarvinUnhelpfulGuide (merleGarvinUnhelpfulGuide) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype MerleGarvinUnhelpfulGuide = MerleGarvinUnhelpfulGuide AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merleGarvinUnhelpfulGuide :: AssetCard MerleGarvinUnhelpfulGuide
merleGarvinUnhelpfulGuide = asset MerleGarvinUnhelpfulGuide Cards.merleGarvinUnhelpfulGuide

instance RunMessage MerleGarvinUnhelpfulGuide where
  runMessage msg (MerleGarvinUnhelpfulGuide attrs) = runQueueT $ case msg of
    _ -> MerleGarvinUnhelpfulGuide <$> liftRunMessage msg attrs
