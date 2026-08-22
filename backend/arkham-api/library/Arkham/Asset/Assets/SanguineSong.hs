module Arkham.Asset.Assets.SanguineSong (sanguineSong) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype SanguineSong = SanguineSong AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanguineSong :: AssetCard SanguineSong
sanguineSong = asset SanguineSong Cards.sanguineSong

instance RunMessage SanguineSong where
  runMessage msg (SanguineSong attrs) = SanguineSong <$> runMessage msg attrs
