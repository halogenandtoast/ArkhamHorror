module Arkham.Asset.Assets.TheGrifterPracticed (theGrifterPracticed) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheGrifterPracticed = TheGrifterPracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGrifterPracticed :: AssetCard TheGrifterPracticed
theGrifterPracticed = asset TheGrifterPracticed Cards.theGrifterPracticed

instance RunMessage TheGrifterPracticed where
  runMessage msg (TheGrifterPracticed attrs) = runQueueT $ case msg of
    _ -> TheGrifterPracticed <$> liftRunMessage msg attrs
