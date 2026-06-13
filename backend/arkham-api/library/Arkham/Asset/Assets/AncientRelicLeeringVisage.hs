module Arkham.Asset.Assets.AncientRelicLeeringVisage (ancientRelic) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype AncientRelicLeeringVisage = AncientRelicLeeringVisage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
ancientRelic :: AssetCard AncientRelicLeeringVisage
ancientRelic = asset AncientRelicLeeringVisage Cards.ancientRelic

instance RunMessage AncientRelicLeeringVisage where
  runMessage msg (AncientRelicLeeringVisage attrs) =
    runQueueT $ AncientRelicLeeringVisage <$> liftRunMessage msg attrs
