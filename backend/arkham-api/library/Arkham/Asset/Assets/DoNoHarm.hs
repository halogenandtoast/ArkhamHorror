module Arkham.Asset.Assets.DoNoHarm (doNoHarm) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DoNoHarm = DoNoHarm AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: Task progression — once per scenario the investigator may attempt their
-- Task objective; marking/erasing progress and the completion rewards are
-- prompted by individual scenarios.
doNoHarm :: AssetCard DoNoHarm
doNoHarm = asset DoNoHarm Cards.doNoHarm

instance RunMessage DoNoHarm where
  runMessage msg (DoNoHarm attrs) = runQueueT $ DoNoHarm <$> liftRunMessage msg attrs
