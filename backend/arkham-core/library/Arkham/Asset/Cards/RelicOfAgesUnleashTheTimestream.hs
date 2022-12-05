module Arkham.Asset.Cards.RelicOfAgesUnleashTheTimestream
  ( relicOfAgesUnleashTheTimestream
  , RelicOfAgesUnleashTheTimestream(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype RelicOfAgesUnleashTheTimestream = RelicOfAgesUnleashTheTimestream AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicOfAgesUnleashTheTimestream :: AssetCard RelicOfAgesUnleashTheTimestream
relicOfAgesUnleashTheTimestream =
  asset RelicOfAgesUnleashTheTimestream Cards.relicOfAgesUnleashTheTimestream

instance RunMessage RelicOfAgesUnleashTheTimestream where
  runMessage msg (RelicOfAgesUnleashTheTimestream attrs) =
    RelicOfAgesUnleashTheTimestream <$> runMessage msg attrs
