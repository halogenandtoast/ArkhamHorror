module Arkham.Location.Cards.LabyrinthineChamber (labyrinthineChamber) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LabyrinthineChamber = LabyrinthineChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

labyrinthineChamber :: LocationCard LabyrinthineChamber
labyrinthineChamber = locationWith LabyrinthineChamber Cards.labyrinthineChamber 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities LabyrinthineChamber where
  getAbilities (LabyrinthineChamber attrs) =
    extendRevealed attrs []

instance RunMessage LabyrinthineChamber where
  runMessage msg (LabyrinthineChamber attrs) = runQueueT $ case msg of
    _ -> LabyrinthineChamber <$> liftRunMessage msg attrs
