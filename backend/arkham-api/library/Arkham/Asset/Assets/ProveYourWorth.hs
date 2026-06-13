module Arkham.Asset.Assets.ProveYourWorth (proveYourWorth) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ProveYourWorth = ProveYourWorth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: Task progression — once per scenario the investigator may attempt their
-- Task objective; marking/erasing progress and the completion rewards are
-- prompted by individual scenarios.
proveYourWorth :: AssetCard ProveYourWorth
proveYourWorth = asset ProveYourWorth Cards.proveYourWorth

instance RunMessage ProveYourWorth where
  runMessage msg (ProveYourWorth attrs) = runQueueT $ ProveYourWorth <$> liftRunMessage msg attrs
