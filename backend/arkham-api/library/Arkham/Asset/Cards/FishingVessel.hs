module Arkham.Asset.Cards.FishingVessel
  ( fishingVessel
  , FishingVessel(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype FishingVessel = FishingVessel AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishingVessel :: AssetCard FishingVessel
fishingVessel = asset FishingVessel Cards.fishingVessel

instance RunMessage FishingVessel where
  runMessage msg (FishingVessel attrs) = runQueueT $ case msg of
    _ -> FishingVessel <$> liftRunMessage msg attrs
