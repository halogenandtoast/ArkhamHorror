module Arkham.Asset.Cards.Observed4 (
  observed4,
  Observed4 (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype Observed4 = Observed4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

observed4 :: AssetCard Observed4
observed4 = asset Observed4 Cards.observed4

instance RunMessage Observed4 where
  runMessage msg (Observed4 attrs) = Observed4 <$> runMessage msg attrs
