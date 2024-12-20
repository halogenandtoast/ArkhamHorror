module Arkham.Asset.Assets.AnyuFaithfulCompanion (anyuFaithfulCompanion) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype AnyuFaithfulCompanion = AnyuFaithfulCompanion AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anyuFaithfulCompanion :: AssetCard AnyuFaithfulCompanion
anyuFaithfulCompanion = asset AnyuFaithfulCompanion Cards.anyuFaithfulCompanion

instance RunMessage AnyuFaithfulCompanion where
  runMessage msg (AnyuFaithfulCompanion attrs) = runQueueT $ case msg of
    _ -> AnyuFaithfulCompanion <$> liftRunMessage msg attrs
