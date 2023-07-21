module Arkham.Asset.Cards.RunicAxe (
  runicAxe,
  RunicAxe (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype RunicAxe = RunicAxe AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runicAxe :: AssetCard RunicAxe
runicAxe = asset RunicAxe Cards.runicAxe

instance RunMessage RunicAxe where
  runMessage msg (RunicAxe attrs) = RunicAxe <$> runMessage msg attrs
