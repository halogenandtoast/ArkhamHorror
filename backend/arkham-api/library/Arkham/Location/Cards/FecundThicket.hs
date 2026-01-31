module Arkham.Location.Cards.FecundThicket (fecundThicket) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FecundThicket = FecundThicket LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fecundThicket :: LocationCard FecundThicket
fecundThicket = locationWith FecundThicket Cards.fecundThicket 6 (PerPlayer 1) connectsToAdjacent

instance HasAbilities FecundThicket where
  getAbilities (FecundThicket a) =
    extendRevealed a []

instance RunMessage FecundThicket where
  runMessage msg (FecundThicket attrs) = runQueueT $ case msg of
    _ -> FecundThicket <$> liftRunMessage msg attrs
