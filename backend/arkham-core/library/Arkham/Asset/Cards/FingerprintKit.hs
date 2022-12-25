module Arkham.Asset.Cards.FingerprintKit
  ( fingerprintKit
  , FingerprintKit(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype FingerprintKit = FingerprintKit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fingerprintKit :: AssetCard FingerprintKit
fingerprintKit = asset FingerprintKit Cards.fingerprintKit

instance RunMessage FingerprintKit where
  runMessage msg (FingerprintKit attrs) =
    FingerprintKit <$> runMessage msg attrs
