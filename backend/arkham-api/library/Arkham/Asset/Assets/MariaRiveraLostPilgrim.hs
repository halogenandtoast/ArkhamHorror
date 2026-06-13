module Arkham.Asset.Assets.MariaRiveraLostPilgrim (mariaRivera) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype MariaRiveraLostPilgrim = MariaRiveraLostPilgrim AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
mariaRivera :: AssetCard MariaRiveraLostPilgrim
mariaRivera = ally MariaRiveraLostPilgrim Cards.mariaRivera (5, 0)

instance RunMessage MariaRiveraLostPilgrim where
  runMessage msg (MariaRiveraLostPilgrim attrs) =
    runQueueT $ MariaRiveraLostPilgrim <$> liftRunMessage msg attrs
