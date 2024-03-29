module Arkham.Asset.Cards.SilassNet
  ( silassNet
  , SilassNet(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype SilassNet = SilassNet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silassNet :: AssetCard SilassNet
silassNet =
  asset SilassNet Cards.silassNet

instance RunMessage SilassNet where
  runMessage msg (SilassNet attrs) = SilassNet <$> runMessage msg attrs
