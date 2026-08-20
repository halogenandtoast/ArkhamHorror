module Arkham.Asset.Assets.DetectiveReynoldsInOverHisHead (detectiveReynoldsInOverHisHead) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DetectiveReynoldsInOverHisHead = DetectiveReynoldsInOverHisHead AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detectiveReynoldsInOverHisHead :: AssetCard DetectiveReynoldsInOverHisHead
detectiveReynoldsInOverHisHead = asset DetectiveReynoldsInOverHisHead Cards.detectiveReynoldsInOverHisHead

instance RunMessage DetectiveReynoldsInOverHisHead where
  runMessage msg (DetectiveReynoldsInOverHisHead attrs) = runQueueT $ case msg of
    _ -> DetectiveReynoldsInOverHisHead <$> liftRunMessage msg attrs
