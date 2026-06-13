module Arkham.Asset.Assets.SkyRelicErodedByWinds (skyRelic) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype SkyRelicErodedByWinds = SkyRelicErodedByWinds AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
skyRelic :: AssetCard SkyRelicErodedByWinds
skyRelic = asset SkyRelicErodedByWinds Cards.skyRelic

instance RunMessage SkyRelicErodedByWinds where
  runMessage msg (SkyRelicErodedByWinds attrs) =
    runQueueT $ SkyRelicErodedByWinds <$> liftRunMessage msg attrs
