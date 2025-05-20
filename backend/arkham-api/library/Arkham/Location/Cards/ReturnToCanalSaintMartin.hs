module Arkham.Location.Cards.ReturnToCanalSaintMartin (returnToCanalSaintMartin) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToCanalSaintMartin = ReturnToCanalSaintMartin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCanalSaintMartin :: LocationCard ReturnToCanalSaintMartin
returnToCanalSaintMartin = location ReturnToCanalSaintMartin Cards.returnToCanalSaintMartin 3 (PerPlayer 1)

instance HasAbilities ReturnToCanalSaintMartin where
  getAbilities (ReturnToCanalSaintMartin attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToCanalSaintMartin where
  runMessage msg (ReturnToCanalSaintMartin attrs) = runQueueT $ case msg of
    _ -> ReturnToCanalSaintMartin <$> liftRunMessage msg attrs
