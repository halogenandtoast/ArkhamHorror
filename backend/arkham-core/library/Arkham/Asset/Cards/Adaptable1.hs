module Arkham.Asset.Cards.Adaptable1 (
  adaptable1,
  Adaptable1 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype Adaptable1 = Adaptable1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

adaptable1 :: AssetCard Adaptable1
adaptable1 = asset Adaptable1 Cards.adaptable1

instance RunMessage Adaptable1 where
  runMessage msg (Adaptable1 attrs) = Adaptable1 <$> runMessage msg attrs
