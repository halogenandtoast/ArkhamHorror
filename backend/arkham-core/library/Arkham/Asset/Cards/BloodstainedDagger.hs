module Arkham.Asset.Cards.BloodstainedDagger
  ( bloodstainedDagger
  , BloodstainedDagger(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype BloodstainedDagger = BloodstainedDagger AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodstainedDagger :: AssetCard BloodstainedDagger
bloodstainedDagger =
  asset BloodstainedDagger Cards.bloodstainedDagger

instance RunMessage BloodstainedDagger where
  runMessage msg (BloodstainedDagger attrs) = BloodstainedDagger <$> runMessage msg attrs
