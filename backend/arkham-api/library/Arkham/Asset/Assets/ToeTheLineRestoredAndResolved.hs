module Arkham.Asset.Assets.ToeTheLineRestoredAndResolved (toeTheLineCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ToeTheLineRestoredAndResolved = ToeTheLineRestoredAndResolved AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
toeTheLineCompleted :: AssetCard ToeTheLineRestoredAndResolved
toeTheLineCompleted = asset ToeTheLineRestoredAndResolved Cards.toeTheLineCompleted

instance RunMessage ToeTheLineRestoredAndResolved where
  runMessage msg (ToeTheLineRestoredAndResolved attrs) =
    runQueueT $ ToeTheLineRestoredAndResolved <$> liftRunMessage msg attrs
