module Arkham.Asset.Assets.TheRedGlovedManHeWasAlwaysThere (theRedGlovedManHeWasAlwaysThere) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheRedGlovedManHeWasAlwaysThere = TheRedGlovedManHeWasAlwaysThere AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedGlovedManHeWasAlwaysThere :: AssetCard TheRedGlovedManHeWasAlwaysThere
theRedGlovedManHeWasAlwaysThere = asset TheRedGlovedManHeWasAlwaysThere Cards.theRedGlovedManHeWasAlwaysThere

instance RunMessage TheRedGlovedManHeWasAlwaysThere where
  runMessage msg (TheRedGlovedManHeWasAlwaysThere attrs) = runQueueT $ case msg of
    _ -> TheRedGlovedManHeWasAlwaysThere <$> liftRunMessage msg attrs
