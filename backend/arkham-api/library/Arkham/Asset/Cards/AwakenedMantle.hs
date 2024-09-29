module Arkham.Asset.Cards.AwakenedMantle
  ( awakenedMantle
  , AwakenedMantle(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype AwakenedMantle = AwakenedMantle AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

awakenedMantle :: AssetCard AwakenedMantle
awakenedMantle = asset AwakenedMantle Cards.awakenedMantle

instance RunMessage AwakenedMantle where
  runMessage msg (AwakenedMantle attrs) = runQueueT $ case msg of
    _ -> AwakenedMantle <$> liftRunMessage msg attrs
