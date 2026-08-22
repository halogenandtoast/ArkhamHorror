module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.HiddenLaboratoryShallowTunnels (hiddenLaboratoryShallowTunnels) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype HiddenLaboratoryShallowTunnels = HiddenLaboratoryShallowTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenLaboratoryShallowTunnels :: LocationCard HiddenLaboratoryShallowTunnels
hiddenLaboratoryShallowTunnels =
  symbolLabel
    $ location HiddenLaboratoryShallowTunnels Cards.hiddenLaboratoryShallowTunnels 4 (PerPlayer 2)

instance HasAbilities HiddenLaboratoryShallowTunnels where
  getAbilities (HiddenLaboratoryShallowTunnels a) = extendRevealed a []

instance RunMessage HiddenLaboratoryShallowTunnels where
  runMessage msg (HiddenLaboratoryShallowTunnels attrs) = runQueueT $ HiddenLaboratoryShallowTunnels <$> liftRunMessage msg attrs
