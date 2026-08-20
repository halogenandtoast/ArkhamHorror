module Arkham.Location.Cards.TheFeastOfHemlockVale.TheThingInTheDepths.MuddyFen (muddyFen) where

import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.CardDefs.TheFeastOfHemlockVale.TheThingInTheDepths qualified as Cards
import Arkham.Location.CardDefs.TheFeastOfHemlockVale.TheThingInTheDepths qualified as Locations
import Arkham.Location.Import.Lifted

newtype MuddyFen = MuddyFen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

muddyFen :: LocationCard MuddyFen
muddyFen = locationWith MuddyFen Cards.muddyFen 4 (PerPlayer 2) connectsToAdjacent

instance HasAbilities MuddyFen where
  getAbilities (MuddyFen a) = extendRevealed a []

instance RunMessage MuddyFen where
  runMessage msg l@(MuddyFen attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.openWater10593b
      pure l
    _ -> MuddyFen <$> liftRunMessage msg attrs
