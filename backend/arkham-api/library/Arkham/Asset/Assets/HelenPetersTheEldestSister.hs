module Arkham.Asset.Assets.HelenPetersTheEldestSister (helenPetersTheEldestSister) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype HelenPetersTheEldestSister = HelenPetersTheEldestSister AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

helenPetersTheEldestSister :: AssetCard HelenPetersTheEldestSister
helenPetersTheEldestSister = asset HelenPetersTheEldestSister Cards.helenPetersTheEldestSister

instance RunMessage HelenPetersTheEldestSister where
  runMessage msg (HelenPetersTheEldestSister attrs) = runQueueT $ case msg of
    _ -> HelenPetersTheEldestSister <$> liftRunMessage msg attrs
