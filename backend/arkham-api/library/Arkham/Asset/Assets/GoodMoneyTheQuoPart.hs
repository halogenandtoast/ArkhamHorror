module Arkham.Asset.Assets.GoodMoneyTheQuoPart (goodMoneyCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype GoodMoneyTheQuoPart = GoodMoneyTheQuoPart AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
goodMoneyCompleted :: AssetCard GoodMoneyTheQuoPart
goodMoneyCompleted = asset GoodMoneyTheQuoPart Cards.goodMoneyCompleted

instance RunMessage GoodMoneyTheQuoPart where
  runMessage msg (GoodMoneyTheQuoPart attrs) =
    runQueueT $ GoodMoneyTheQuoPart <$> liftRunMessage msg attrs
