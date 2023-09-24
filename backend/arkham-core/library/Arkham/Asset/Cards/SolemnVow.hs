module Arkham.Asset.Cards.SolemnVow (
  solemnVow,
  SolemnVow (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype SolemnVow = SolemnVow AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

solemnVow :: AssetCard SolemnVow
solemnVow = asset SolemnVow Cards.solemnVow

instance RunMessage SolemnVow where
  runMessage msg (SolemnVow attrs) = SolemnVow <$> runMessage msg attrs
