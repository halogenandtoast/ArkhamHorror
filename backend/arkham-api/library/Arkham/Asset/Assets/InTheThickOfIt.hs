module Arkham.Asset.Assets.InTheThickOfIt (inTheThickOfIt, InTheThickOfIt (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype InTheThickOfIt = InTheThickOfIt AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheThickOfIt :: AssetCard InTheThickOfIt
inTheThickOfIt = asset InTheThickOfIt Cards.inTheThickOfIt

instance RunMessage InTheThickOfIt where
  runMessage msg (InTheThickOfIt attrs) = InTheThickOfIt <$> runMessage msg attrs
