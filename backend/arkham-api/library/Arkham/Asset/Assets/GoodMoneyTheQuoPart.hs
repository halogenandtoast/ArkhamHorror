module Arkham.Asset.Assets.GoodMoneyTheQuoPart (goodMoneyCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWith_, setActiveDuringSetup)

newtype GoodMoneyTheQuoPart = GoodMoneyTheQuoPart AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goodMoneyCompleted :: AssetCard GoodMoneyTheQuoPart
goodMoneyCompleted = asset GoodMoneyTheQuoPart Cards.goodMoneyCompleted

instance HasModifiersFor GoodMoneyTheQuoPart where
  getModifiersFor (GoodMoneyTheQuoPart a) = for_ a.controller \iid ->
    modifiedWith_ a iid setActiveDuringSetup [StartingResources 5]

instance RunMessage GoodMoneyTheQuoPart where
  runMessage msg (GoodMoneyTheQuoPart attrs) =
    runQueueT $ GoodMoneyTheQuoPart <$> liftRunMessage msg attrs
