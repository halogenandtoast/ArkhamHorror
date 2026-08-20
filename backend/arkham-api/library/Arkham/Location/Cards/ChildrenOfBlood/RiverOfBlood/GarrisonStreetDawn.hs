module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.GarrisonStreetDawn (garrisonStreetDawn) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype GarrisonStreetDawn = GarrisonStreetDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

garrisonStreetDawn :: LocationCard GarrisonStreetDawn
garrisonStreetDawn = symbolLabel $ location GarrisonStreetDawn Cards.garrisonStreetDawn 3 (PerPlayer 1)

instance HasAbilities GarrisonStreetDawn where
  getAbilities (GarrisonStreetDawn a) =
    extendRevealed a []

instance RunMessage GarrisonStreetDawn where
  runMessage msg (GarrisonStreetDawn attrs) = runQueueT $ case msg of
    _ -> GarrisonStreetDawn <$> liftRunMessage msg attrs
