module Arkham.Location.Cards.Pasture (pasture) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Pasture = Pasture LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pasture :: LocationCard Pasture
pasture = symbolLabel $ locationWith Pasture Cards.pasture 0 (Static 0) connectsToAdjacent

instance HasAbilities Pasture where
  getAbilities (Pasture a) =
    extendRevealed a []

instance RunMessage Pasture where
  runMessage msg (Pasture attrs) = runQueueT $ case msg of
    _ -> Pasture <$> liftRunMessage msg attrs
