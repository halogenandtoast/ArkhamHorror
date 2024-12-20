module Arkham.Asset.Assets.CollectedWorksOfPoe (collectedWorksOfPoe) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype CollectedWorksOfPoe = CollectedWorksOfPoe AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collectedWorksOfPoe :: AssetCard CollectedWorksOfPoe
collectedWorksOfPoe = asset CollectedWorksOfPoe Cards.collectedWorksOfPoe

instance RunMessage CollectedWorksOfPoe where
  runMessage msg (CollectedWorksOfPoe attrs) = runQueueT $ case msg of
    _ -> CollectedWorksOfPoe <$> liftRunMessage msg attrs
