module Arkham.Asset.Assets.KeyLocusLastBastion (keyLocusLastBastion) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype KeyLocusLastBastion = KeyLocusLastBastion AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyLocusLastBastion :: AssetCard KeyLocusLastBastion
keyLocusLastBastion = asset KeyLocusLastBastion Cards.keyLocusLastBastion

instance RunMessage KeyLocusLastBastion where
  runMessage msg (KeyLocusLastBastion attrs) = runQueueT $ case msg of
    _ -> KeyLocusLastBastion <$> liftRunMessage msg attrs
