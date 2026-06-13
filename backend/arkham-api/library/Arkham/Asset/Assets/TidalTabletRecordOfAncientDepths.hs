module Arkham.Asset.Assets.TidalTabletRecordOfAncientDepths (tidalTablet) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TidalTabletRecordOfAncientDepths = TidalTabletRecordOfAncientDepths AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
tidalTablet :: AssetCard TidalTabletRecordOfAncientDepths
tidalTablet = asset TidalTabletRecordOfAncientDepths Cards.tidalTablet

instance RunMessage TidalTabletRecordOfAncientDepths where
  runMessage msg (TidalTabletRecordOfAncientDepths attrs) =
    runQueueT $ TidalTabletRecordOfAncientDepths <$> liftRunMessage msg attrs
