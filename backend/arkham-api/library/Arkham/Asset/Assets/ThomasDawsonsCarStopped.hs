module Arkham.Asset.Assets.ThomasDawsonsCarStopped
  ( thomasDawsonsCarStopped
  , ThomasDawsonsCarStopped(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ThomasDawsonsCarStopped = ThomasDawsonsCarStopped AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasDawsonsCarStopped :: AssetCard ThomasDawsonsCarStopped
thomasDawsonsCarStopped = asset ThomasDawsonsCarStopped Cards.thomasDawsonsCarStopped

instance RunMessage ThomasDawsonsCarStopped where
  runMessage msg (ThomasDawsonsCarStopped attrs) = runQueueT $ case msg of
    _ -> ThomasDawsonsCarStopped <$> liftRunMessage msg attrs
