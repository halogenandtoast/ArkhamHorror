module Arkham.Asset.Assets.DreamsOfDestruction (dreamsOfDestruction) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DreamsOfDestruction = DreamsOfDestruction AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: Task progression — once per scenario the investigator may attempt their
-- Task objective; marking/erasing progress and the completion rewards are
-- prompted by individual scenarios.
dreamsOfDestruction :: AssetCard DreamsOfDestruction
dreamsOfDestruction = asset DreamsOfDestruction Cards.dreamsOfDestruction

instance RunMessage DreamsOfDestruction where
  runMessage msg (DreamsOfDestruction attrs) = runQueueT $ DreamsOfDestruction <$> liftRunMessage msg attrs
