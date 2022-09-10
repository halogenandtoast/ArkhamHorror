module Arkham.Asset.Cards.ShrewdAnalysis
  ( shrewdAnalysis
  , ShrewdAnalysis(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype ShrewdAnalysis = ShrewdAnalysis AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrewdAnalysis :: AssetCard ShrewdAnalysis
shrewdAnalysis = asset ShrewdAnalysis Cards.shrewdAnalysis

instance RunMessage ShrewdAnalysis where
  runMessage msg (ShrewdAnalysis attrs) =
    ShrewdAnalysis <$> runMessage msg attrs
