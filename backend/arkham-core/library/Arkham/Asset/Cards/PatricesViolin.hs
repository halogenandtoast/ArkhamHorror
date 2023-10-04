module Arkham.Asset.Cards.PatricesViolin
  ( patricesViolin
  , PatricesViolin(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype PatricesViolin = PatricesViolin AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patricesViolin :: AssetCard PatricesViolin
patricesViolin =
  asset PatricesViolin Cards.patricesViolin

instance RunMessage PatricesViolin where
  runMessage msg (PatricesViolin attrs) = PatricesViolin <$> runMessage msg attrs
