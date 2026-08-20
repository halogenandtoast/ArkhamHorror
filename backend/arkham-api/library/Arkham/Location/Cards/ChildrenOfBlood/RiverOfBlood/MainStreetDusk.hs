module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.MainStreetDusk (mainStreetDusk) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype MainStreetDusk = MainStreetDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainStreetDusk :: LocationCard MainStreetDusk
mainStreetDusk = symbolLabel $ location MainStreetDusk Cards.mainStreetDusk 3 (PerPlayer 2)

instance HasAbilities MainStreetDusk where
  getAbilities (MainStreetDusk a) =
    extendRevealed a []

instance RunMessage MainStreetDusk where
  runMessage msg (MainStreetDusk attrs) = runQueueT $ case msg of
    _ -> MainStreetDusk <$> liftRunMessage msg attrs
