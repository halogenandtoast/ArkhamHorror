module Arkham.Asset.Cards.SeaChangeHarpoon
  ( seaChangeHarpoon
  , SeaChangeHarpoon(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype SeaChangeHarpoon = SeaChangeHarpoon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaChangeHarpoon :: AssetCard SeaChangeHarpoon
seaChangeHarpoon =
  asset SeaChangeHarpoon Cards.seaChangeHarpoon

instance RunMessage SeaChangeHarpoon where
  runMessage msg (SeaChangeHarpoon attrs) = SeaChangeHarpoon <$> runMessage msg attrs
