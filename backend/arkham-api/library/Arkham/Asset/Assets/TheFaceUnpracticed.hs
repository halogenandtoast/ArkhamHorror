module Arkham.Asset.Assets.TheFaceUnpracticed (theFaceUnpracticed) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheFaceUnpracticed = TheFaceUnpracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFaceUnpracticed :: AssetCard TheFaceUnpracticed
theFaceUnpracticed = asset TheFaceUnpracticed Cards.theFaceUnpracticed

instance RunMessage TheFaceUnpracticed where
  runMessage msg (TheFaceUnpracticed attrs) = runQueueT $ case msg of
    _ -> TheFaceUnpracticed <$> liftRunMessage msg attrs
