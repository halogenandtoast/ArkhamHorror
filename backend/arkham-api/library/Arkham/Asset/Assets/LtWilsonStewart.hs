module Arkham.Asset.Assets.LtWilsonStewart (ltWilsonStewart) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype LtWilsonStewart = LtWilsonStewart AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ltWilsonStewart :: AssetCard LtWilsonStewart
ltWilsonStewart = ally LtWilsonStewart Cards.ltWilsonStewart (2, 2)

instance RunMessage LtWilsonStewart where
  runMessage msg (LtWilsonStewart attrs) = LtWilsonStewart <$> runMessage msg attrs
