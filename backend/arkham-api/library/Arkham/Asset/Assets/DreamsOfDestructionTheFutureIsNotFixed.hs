module Arkham.Asset.Assets.DreamsOfDestructionTheFutureIsNotFixed (dreamsOfDestructionCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DreamsOfDestructionTheFutureIsNotFixed = DreamsOfDestructionTheFutureIsNotFixed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
dreamsOfDestructionCompleted :: AssetCard DreamsOfDestructionTheFutureIsNotFixed
dreamsOfDestructionCompleted =
  asset DreamsOfDestructionTheFutureIsNotFixed Cards.dreamsOfDestructionCompleted

instance RunMessage DreamsOfDestructionTheFutureIsNotFixed where
  runMessage msg (DreamsOfDestructionTheFutureIsNotFixed attrs) =
    runQueueT $ DreamsOfDestructionTheFutureIsNotFixed <$> liftRunMessage msg attrs
