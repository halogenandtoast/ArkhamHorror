module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.WaterStreetDusk (waterStreetDusk) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype WaterStreetDusk = WaterStreetDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterStreetDusk :: LocationCard WaterStreetDusk
waterStreetDusk = symbolLabel $ location WaterStreetDusk Cards.waterStreetDusk 3 (PerPlayer 1)

instance HasAbilities WaterStreetDusk where
  getAbilities (WaterStreetDusk a) =
    extendRevealed a []

instance RunMessage WaterStreetDusk where
  runMessage msg (WaterStreetDusk attrs) = runQueueT $ case msg of
    _ -> WaterStreetDusk <$> liftRunMessage msg attrs
