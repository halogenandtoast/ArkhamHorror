module Arkham.Asset.Assets.NoPlaceLikeHomeWhereYourHeartIs (noPlaceLikeHomeCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype NoPlaceLikeHomeWhereYourHeartIs = NoPlaceLikeHomeWhereYourHeartIs AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
noPlaceLikeHomeCompleted :: AssetCard NoPlaceLikeHomeWhereYourHeartIs
noPlaceLikeHomeCompleted = asset NoPlaceLikeHomeWhereYourHeartIs Cards.noPlaceLikeHomeCompleted

instance RunMessage NoPlaceLikeHomeWhereYourHeartIs where
  runMessage msg (NoPlaceLikeHomeWhereYourHeartIs attrs) =
    runQueueT $ NoPlaceLikeHomeWhereYourHeartIs <$> liftRunMessage msg attrs
