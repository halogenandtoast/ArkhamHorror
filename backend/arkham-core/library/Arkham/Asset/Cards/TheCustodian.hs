module Arkham.Asset.Cards.TheCustodian
  ( theCustodian
  , TheCustodian(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype TheCustodian = TheCustodian AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCustodian :: AssetCard TheCustodian
theCustodian = asset TheCustodian Cards.theCustodian

instance RunMessage TheCustodian where
  runMessage msg (TheCustodian attrs) = TheCustodian <$> runMessage msg attrs
