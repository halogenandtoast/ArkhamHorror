module Arkham.Asset.Assets.Collector (collector) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype Collector = Collector AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collector :: AssetCard Collector
collector = asset Collector Cards.collector

instance RunMessage Collector where
  runMessage msg (Collector attrs) = runQueueT $ Collector <$> liftRunMessage msg attrs
