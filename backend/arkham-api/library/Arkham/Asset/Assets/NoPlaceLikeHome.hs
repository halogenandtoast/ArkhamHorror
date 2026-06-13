module Arkham.Asset.Assets.NoPlaceLikeHome (noPlaceLikeHome) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype NoPlaceLikeHome = NoPlaceLikeHome AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: Task progression — once per scenario the investigator may attempt their
-- Task objective; marking/erasing progress and the completion rewards are
-- prompted by individual scenarios.
noPlaceLikeHome :: AssetCard NoPlaceLikeHome
noPlaceLikeHome = asset NoPlaceLikeHome Cards.noPlaceLikeHome

instance RunMessage NoPlaceLikeHome where
  runMessage msg (NoPlaceLikeHome attrs) = runQueueT $ NoPlaceLikeHome <$> liftRunMessage msg attrs
