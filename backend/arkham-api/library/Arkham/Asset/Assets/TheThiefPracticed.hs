module Arkham.Asset.Assets.TheThiefPracticed (theThiefPracticed) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheThiefPracticed = TheThiefPracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThiefPracticed :: AssetCard TheThiefPracticed
theThiefPracticed = asset TheThiefPracticed Cards.theThiefPracticed

instance RunMessage TheThiefPracticed where
  runMessage msg (TheThiefPracticed attrs) = runQueueT $ case msg of
    _ -> TheThiefPracticed <$> liftRunMessage msg attrs
