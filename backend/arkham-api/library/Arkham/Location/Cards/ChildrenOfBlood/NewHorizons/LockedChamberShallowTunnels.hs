module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.LockedChamberShallowTunnels (lockedChamberShallowTunnels) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype LockedChamberShallowTunnels = LockedChamberShallowTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedChamberShallowTunnels :: LocationCard LockedChamberShallowTunnels
lockedChamberShallowTunnels =
  symbolLabel $ location LockedChamberShallowTunnels Cards.lockedChamberShallowTunnels 3 (PerPlayer 2)

instance HasAbilities LockedChamberShallowTunnels where
  getAbilities (LockedChamberShallowTunnels a) = extendRevealed a []

instance RunMessage LockedChamberShallowTunnels where
  runMessage msg (LockedChamberShallowTunnels attrs) = runQueueT $ LockedChamberShallowTunnels <$> liftRunMessage msg attrs
