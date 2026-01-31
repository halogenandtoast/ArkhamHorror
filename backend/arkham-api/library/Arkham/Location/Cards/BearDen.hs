module Arkham.Location.Cards.BearDen (bearDen) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BearDen = BearDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bearDen :: LocationCard BearDen
bearDen = locationWith BearDen Cards.bearDen 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities BearDen where
  getAbilities (BearDen a) =
    extendRevealed a []

instance RunMessage BearDen where
  runMessage msg (BearDen attrs) = runQueueT $ case msg of
    _ -> BearDen <$> liftRunMessage msg attrs
