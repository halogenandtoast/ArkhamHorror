module Arkham.Asset.Assets.GoodMoney (goodMoney) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype GoodMoney = GoodMoney AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: Task progression — once per scenario the investigator may attempt their
-- Task objective; marking/erasing progress and the completion rewards are
-- prompted by individual scenarios.
goodMoney :: AssetCard GoodMoney
goodMoney = asset GoodMoney Cards.goodMoney

instance RunMessage GoodMoney where
  runMessage msg (GoodMoney attrs) = runQueueT $ GoodMoney <$> liftRunMessage msg attrs
