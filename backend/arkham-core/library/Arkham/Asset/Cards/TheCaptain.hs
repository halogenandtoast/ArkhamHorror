module Arkham.Asset.Cards.TheCaptain
  ( theCaptain
  , TheCaptain(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype TheCaptain = TheCaptain AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCaptain :: AssetCard TheCaptain
theCaptain =
  asset TheCaptain Cards.theCaptain

instance RunMessage TheCaptain where
  runMessage msg (TheCaptain attrs) = TheCaptain <$> runMessage msg attrs
