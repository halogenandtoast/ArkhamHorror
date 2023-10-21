module Arkham.Asset.Cards.SergeantMonroe (
  sergeantMonroe,
  SergeantMonroe (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype SergeantMonroe = SergeantMonroe AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sergeantMonroe :: AssetCard SergeantMonroe
sergeantMonroe = ally SergeantMonroe Cards.sergeantMonroe (3, 3)

instance RunMessage SergeantMonroe where
  runMessage msg (SergeantMonroe attrs) = SergeantMonroe <$> runMessage msg attrs
