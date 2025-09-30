module Arkham.Location.Cards.ReturnToSouthside (returnToSouthside) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToSouthside = ReturnToSouthside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToSouthside :: LocationCard ReturnToSouthside
returnToSouthside = location ReturnToSouthside Cards.returnToSouthside 3 (Static 0)

instance HasAbilities ReturnToSouthside where
  getAbilities (ReturnToSouthside attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToSouthside where
  runMessage msg (ReturnToSouthside attrs) = runQueueT $ case msg of
    _ -> ReturnToSouthside <$> liftRunMessage msg attrs
