module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.CavernEntranceShallowTunnels (cavernEntranceShallowTunnels) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype CavernEntranceShallowTunnels = CavernEntranceShallowTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernEntranceShallowTunnels :: LocationCard CavernEntranceShallowTunnels
cavernEntranceShallowTunnels =
  symbolLabel
    $ location CavernEntranceShallowTunnels Cards.cavernEntranceShallowTunnels 2 (PerPlayer 1)

instance HasAbilities CavernEntranceShallowTunnels where
  getAbilities (CavernEntranceShallowTunnels a) = extendRevealed a []

instance RunMessage CavernEntranceShallowTunnels where
  runMessage msg (CavernEntranceShallowTunnels attrs) = runQueueT $ CavernEntranceShallowTunnels <$> liftRunMessage msg attrs
