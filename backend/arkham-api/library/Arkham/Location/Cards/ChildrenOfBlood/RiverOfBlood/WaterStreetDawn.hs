module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.WaterStreetDawn (waterStreetDawn) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype WaterStreetDawn = WaterStreetDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterStreetDawn :: LocationCard WaterStreetDawn
waterStreetDawn = symbolLabel $ location WaterStreetDawn Cards.waterStreetDawn 2 (PerPlayer 1)

instance HasAbilities WaterStreetDawn where
  getAbilities (WaterStreetDawn a) =
    extendRevealed a []

instance RunMessage WaterStreetDawn where
  runMessage msg (WaterStreetDawn attrs) = runQueueT $ case msg of
    _ -> WaterStreetDawn <$> liftRunMessage msg attrs
