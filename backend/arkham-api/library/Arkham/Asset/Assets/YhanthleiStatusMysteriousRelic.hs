module Arkham.Asset.Assets.YhanthleiStatusMysteriousRelic
  ( yhanthleiStatusMysteriousRelic
  , YhanthleiStatusMysteriousRelic(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype YhanthleiStatusMysteriousRelic = YhanthleiStatusMysteriousRelic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yhanthleiStatusMysteriousRelic :: AssetCard YhanthleiStatusMysteriousRelic
yhanthleiStatusMysteriousRelic = asset YhanthleiStatusMysteriousRelic Cards.yhanthleiStatusMysteriousRelic

instance RunMessage YhanthleiStatusMysteriousRelic where
  runMessage msg (YhanthleiStatusMysteriousRelic attrs) = runQueueT $ case msg of
    _ -> YhanthleiStatusMysteriousRelic <$> liftRunMessage msg attrs
