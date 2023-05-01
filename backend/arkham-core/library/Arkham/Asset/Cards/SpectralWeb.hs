module Arkham.Asset.Cards.SpectralWeb
  ( spectralWeb
  , SpectralWeb(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype SpectralWeb = SpectralWeb AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralWeb :: AssetCard SpectralWeb
spectralWeb =
  asset SpectralWeb Cards.spectralWeb

instance RunMessage SpectralWeb where
  runMessage msg (SpectralWeb attrs) = SpectralWeb <$> runMessage msg attrs
