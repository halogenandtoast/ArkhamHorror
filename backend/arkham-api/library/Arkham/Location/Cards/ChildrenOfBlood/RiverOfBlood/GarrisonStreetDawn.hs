module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.GarrisonStreetDawn (garrisonStreetDawn) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype GarrisonStreetDawn = GarrisonStreetDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

garrisonStreetDawn :: LocationCard GarrisonStreetDawn
garrisonStreetDawn = symbolLabel $ location GarrisonStreetDawn Cards.garrisonStreetDawn 3 (PerPlayer 1)

instance HasAbilities GarrisonStreetDawn where
  getAbilities (GarrisonStreetDawn a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> DuringTurn You
            <> thisIs a LocationWithoutEnemies
            <> CanMoveTo (ConnectedFrom ForMovement (be a))
        )
      $ FastAbility Free

instance RunMessage GarrisonStreetDawn where
  runMessage msg l@(GarrisonStreetDawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      choices <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseTargetM iid choices $ moveTo (attrs.ability 1) iid
      pure l
    _ -> GarrisonStreetDawn <$> liftRunMessage msg attrs
