module Arkham.Asset.Assets.MeteoriteSample (meteoriteSample) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype MeteoriteSample = MeteoriteSample AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meteoriteSample :: AssetCard MeteoriteSample
meteoriteSample = asset MeteoriteSample Cards.meteoriteSample

instance RunMessage MeteoriteSample where
  runMessage msg (MeteoriteSample attrs) = MeteoriteSample <$> runMessage msg attrs
