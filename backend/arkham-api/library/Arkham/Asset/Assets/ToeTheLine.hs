module Arkham.Asset.Assets.ToeTheLine (toeTheLine) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ToeTheLine = ToeTheLine AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: Task progression — once per scenario the investigator may attempt their
-- Task objective; marking/erasing progress and the completion rewards are
-- prompted by individual scenarios.
toeTheLine :: AssetCard ToeTheLine
toeTheLine = asset ToeTheLine Cards.toeTheLine

instance RunMessage ToeTheLine where
  runMessage msg (ToeTheLine attrs) = runQueueT $ ToeTheLine <$> liftRunMessage msg attrs
