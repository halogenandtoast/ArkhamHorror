module Arkham.Asset.Assets.YhanthleiStatusDynamicRelic
  ( yhanthleiStatusDynamicRelic
  , YhanthleiStatusDynamicRelic(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype YhanthleiStatusDynamicRelic = YhanthleiStatusDynamicRelic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yhanthleiStatusDynamicRelic :: AssetCard YhanthleiStatusDynamicRelic
yhanthleiStatusDynamicRelic = asset YhanthleiStatusDynamicRelic Cards.yhanthleiStatusDynamicRelic

instance RunMessage YhanthleiStatusDynamicRelic where
  runMessage msg (YhanthleiStatusDynamicRelic attrs) = runQueueT $ case msg of
    _ -> YhanthleiStatusDynamicRelic <$> liftRunMessage msg attrs
