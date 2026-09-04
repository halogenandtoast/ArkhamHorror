module Arkham.Asset.Assets.ShrewdAnalysis (shrewdAnalysis) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype ShrewdAnalysis = ShrewdAnalysis AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrewdAnalysis :: AssetCard ShrewdAnalysis
shrewdAnalysis = asset ShrewdAnalysis Cards.shrewdAnalysis
