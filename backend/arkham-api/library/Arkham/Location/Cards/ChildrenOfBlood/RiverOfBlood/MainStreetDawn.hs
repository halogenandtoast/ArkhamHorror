module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.MainStreetDawn (mainStreetDawn) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype MainStreetDawn = MainStreetDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainStreetDawn :: LocationCard MainStreetDawn
mainStreetDawn = symbolLabel $ location MainStreetDawn Cards.mainStreetDawn 3 (PerPlayer 2)

instance HasAbilities MainStreetDawn where
  getAbilities (MainStreetDawn a) =
    extendRevealed a []

instance RunMessage MainStreetDawn where
  runMessage msg (MainStreetDawn attrs) = runQueueT $ case msg of
    _ -> MainStreetDawn <$> liftRunMessage msg attrs
