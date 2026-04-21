module Arkham.Location.Cards.FoyerHemlockHouse (foyerHemlockHouse) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FoyerHemlockHouse = FoyerHemlockHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyerHemlockHouse :: LocationCard FoyerHemlockHouse
foyerHemlockHouse = locationWith FoyerHemlockHouse Cards.foyerHemlockHouse 2 (PerPlayer 0) connectsToAdjacent

instance HasAbilities FoyerHemlockHouse where
  getAbilities (FoyerHemlockHouse a) =
    extendRevealed a []

instance RunMessage FoyerHemlockHouse where
  runMessage msg (FoyerHemlockHouse attrs) = runQueueT $ case msg of
    _ -> FoyerHemlockHouse <$> liftRunMessage msg attrs
