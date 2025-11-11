module Arkham.Asset.Assets.TheGrifterUnpracticed (theGrifterUnpracticed) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheGrifterUnpracticed = TheGrifterUnpracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGrifterUnpracticed :: AssetCard TheGrifterUnpracticed
theGrifterUnpracticed = asset TheGrifterUnpracticed Cards.theGrifterUnpracticed

instance RunMessage TheGrifterUnpracticed where
  runMessage msg (TheGrifterUnpracticed attrs) = runQueueT $ case msg of
    _ -> TheGrifterUnpracticed <$> liftRunMessage msg attrs
