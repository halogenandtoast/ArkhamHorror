module Arkham.Asset.Assets.TheThiefUnpracticed (theThiefUnpracticed) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheThiefUnpracticed = TheThiefUnpracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThiefUnpracticed :: AssetCard TheThiefUnpracticed
theThiefUnpracticed = asset TheThiefUnpracticed Cards.theThiefUnpracticed

instance RunMessage TheThiefUnpracticed where
  runMessage msg (TheThiefUnpracticed attrs) = runQueueT $ case msg of
    _ -> TheThiefUnpracticed <$> liftRunMessage msg attrs
