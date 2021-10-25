module Arkham.Types.Asset.Cards.DanielChesterfield
  ( danielChesterfield
  , DanielChesterfield(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype DanielChesterfield = DanielChesterfield AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielChesterfield :: AssetCard DanielChesterfield
danielChesterfield = asset DanielChesterfield Cards.danielChesterfield

instance AssetRunner env => RunMessage env DanielChesterfield where
  runMessage msg (DanielChesterfield attrs) =
    DanielChesterfield <$> runMessage msg attrs
