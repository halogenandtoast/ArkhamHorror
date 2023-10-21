module Arkham.Asset.Cards.AlienDevice (
  alienDevice,
  AlienDevice (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype AlienDevice = AlienDevice AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienDevice :: AssetCard AlienDevice
alienDevice = asset AlienDevice Cards.alienDevice

instance RunMessage AlienDevice where
  runMessage msg a@(AlienDevice attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> AlienDevice <$> runMessage msg attrs
