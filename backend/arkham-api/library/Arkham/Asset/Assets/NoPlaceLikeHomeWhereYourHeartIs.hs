module Arkham.Asset.Assets.NoPlaceLikeHomeWhereYourHeartIs (noPlaceLikeHomeCompleted) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)

newtype NoPlaceLikeHomeWhereYourHeartIs = NoPlaceLikeHomeWhereYourHeartIs AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noPlaceLikeHomeCompleted :: AssetCard NoPlaceLikeHomeWhereYourHeartIs
noPlaceLikeHomeCompleted = asset NoPlaceLikeHomeWhereYourHeartIs Cards.noPlaceLikeHomeCompleted

instance HasModifiersFor NoPlaceLikeHomeWhereYourHeartIs where
  getModifiersFor (NoPlaceLikeHomeWhereYourHeartIs a) =
    controllerGets
      a
      [ GiveAdditionalAction
          $ AdditionalAction "No Place Like Home" (toSource a) (ActionRestrictedAdditionalAction #move)
      ]

instance RunMessage NoPlaceLikeHomeWhereYourHeartIs where
  runMessage msg (NoPlaceLikeHomeWhereYourHeartIs attrs) =
    runQueueT $ NoPlaceLikeHomeWhereYourHeartIs <$> liftRunMessage msg attrs
