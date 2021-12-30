module Arkham.Asset.Cards.Courage
  ( courage
  , Courage(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs

newtype Courage = Courage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courage :: AssetCard Courage
courage = assetWith Courage Cards.courage (sanityL ?~ 2)

instance AssetRunner env => RunMessage env Courage where
  runMessage msg (Courage attrs) = Courage <$> runMessage msg attrs
