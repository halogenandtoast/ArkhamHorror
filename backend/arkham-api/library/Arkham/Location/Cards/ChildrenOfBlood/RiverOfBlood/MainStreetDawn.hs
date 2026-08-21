module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.MainStreetDawn (mainStreetDawn) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MainStreetDawn = MainStreetDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainStreetDawn :: LocationCard MainStreetDawn
mainStreetDawn = symbolLabel $ location MainStreetDawn Cards.mainStreetDawn 3 (PerPlayer 2)

instance HasAbilities MainStreetDawn where
  getAbilities (MainStreetDawn a) =
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

instance RunMessage MainStreetDawn where
  runMessage msg l@(MainStreetDawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      choices <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseTargetM iid choices $ moveTo (attrs.ability 1) iid
      pure l
    _ -> MainStreetDawn <$> liftRunMessage msg attrs
