module Arkham.Asset.Assets.OldMemory (oldMemory) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype OldMemory = OldMemory AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldMemory :: AssetCard OldMemory
oldMemory = asset OldMemory Cards.oldMemory

instance RunMessage OldMemory where
  runMessage msg (OldMemory attrs) =
    runQueueT $ OldMemory <$> liftRunMessage msg attrs
