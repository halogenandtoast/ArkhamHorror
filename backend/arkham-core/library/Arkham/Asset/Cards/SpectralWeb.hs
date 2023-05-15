module Arkham.Asset.Cards.SpectralWeb (
  spectralWeb,
  SpectralWeb (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype SpectralWeb = SpectralWeb AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralWeb :: AssetCard SpectralWeb
spectralWeb =
  asset SpectralWeb Cards.spectralWeb

instance HasAbilities SpectralWeb where
  getAbilities (SpectralWeb attrs) =
    [ restrictedAbility attrs 1 ControlsThis $
        ActionAbility Nothing $
          ActionCost 1 <> GroupClueCostRange (1, 3) YourLocation
    ]

instance RunMessage SpectralWeb where
  runMessage msg (SpectralWeb attrs) = SpectralWeb <$> runMessage msg attrs
