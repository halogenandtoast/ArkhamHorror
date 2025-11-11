module Arkham.Asset.Assets.TheFacePracticed (theFacePracticed) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheFacePracticed = TheFacePracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFacePracticed :: AssetCard TheFacePracticed
theFacePracticed = asset TheFacePracticed Cards.theFacePracticed

instance RunMessage TheFacePracticed where
  runMessage msg (TheFacePracticed attrs) = runQueueT $ case msg of
    _ -> TheFacePracticed <$> liftRunMessage msg attrs
