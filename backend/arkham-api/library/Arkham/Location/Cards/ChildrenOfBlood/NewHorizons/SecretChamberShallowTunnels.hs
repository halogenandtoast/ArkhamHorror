module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.SecretChamberShallowTunnels (secretChamberShallowTunnels) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype SecretChamberShallowTunnels = SecretChamberShallowTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretChamberShallowTunnels :: LocationCard SecretChamberShallowTunnels
secretChamberShallowTunnels =
  symbolLabel $ location SecretChamberShallowTunnels Cards.secretChamberShallowTunnels 2 (PerPlayer 2)

instance HasAbilities SecretChamberShallowTunnels where
  getAbilities (SecretChamberShallowTunnels a) = extendRevealed a []

instance RunMessage SecretChamberShallowTunnels where
  runMessage msg (SecretChamberShallowTunnels attrs) = runQueueT $ SecretChamberShallowTunnels <$> liftRunMessage msg attrs
