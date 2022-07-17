module Arkham.Asset.Cards.MistsOfRlyeh
  ( mistsOfRlyeh
  , MistsOfRlyeh(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype MistsOfRlyeh = MistsOfRlyeh AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh :: AssetCard MistsOfRlyeh
mistsOfRlyeh = asset MistsOfRlyeh Cards.mistsOfRlyeh

instance RunMessage MistsOfRlyeh where
  runMessage msg (MistsOfRlyeh attrs) = MistsOfRlyeh <$> runMessage msg attrs
