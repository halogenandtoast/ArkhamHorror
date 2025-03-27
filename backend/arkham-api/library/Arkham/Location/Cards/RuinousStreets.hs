module Arkham.Location.Cards.RuinousStreets (ruinousStreets) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RuinousStreets = RuinousStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinousStreets :: LocationCard RuinousStreets
ruinousStreets = locationWith RuinousStreets Cards.ruinousStreets 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RuinousStreets where
  getAbilities (RuinousStreets attrs) =
    extendRevealed attrs []

instance RunMessage RuinousStreets where
  runMessage msg (RuinousStreets attrs) = runQueueT $ case msg of
    _ -> RuinousStreets <$> liftRunMessage msg attrs
