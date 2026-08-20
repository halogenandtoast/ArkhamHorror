module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.GarrisonStreetDusk (garrisonStreetDusk) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype GarrisonStreetDusk = GarrisonStreetDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

garrisonStreetDusk :: LocationCard GarrisonStreetDusk
garrisonStreetDusk = symbolLabel $ location GarrisonStreetDusk Cards.garrisonStreetDusk 3 (PerPlayer 1)

instance HasAbilities GarrisonStreetDusk where
  getAbilities (GarrisonStreetDusk a) =
    extendRevealed a []

instance RunMessage GarrisonStreetDusk where
  runMessage msg (GarrisonStreetDusk attrs) = runQueueT $ case msg of
    _ -> GarrisonStreetDusk <$> liftRunMessage msg attrs
