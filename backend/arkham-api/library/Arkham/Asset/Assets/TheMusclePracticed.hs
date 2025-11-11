module Arkham.Asset.Assets.TheMusclePracticed (theMusclePracticed) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheMusclePracticed = TheMusclePracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMusclePracticed :: AssetCard TheMusclePracticed
theMusclePracticed = asset TheMusclePracticed Cards.theMusclePracticed

instance RunMessage TheMusclePracticed where
  runMessage msg (TheMusclePracticed attrs) = runQueueT $ case msg of
    _ -> TheMusclePracticed <$> liftRunMessage msg attrs
