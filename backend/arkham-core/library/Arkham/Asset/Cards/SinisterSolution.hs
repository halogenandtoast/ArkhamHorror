module Arkham.Asset.Cards.SinisterSolution (
  sinisterSolution,
  SinisterSolution (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype SinisterSolution = SinisterSolution AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinisterSolution :: AssetCard SinisterSolution
sinisterSolution = asset SinisterSolution Cards.sinisterSolution

instance RunMessage SinisterSolution where
  runMessage msg a@(SinisterSolution attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> SinisterSolution <$> runMessage msg attrs
