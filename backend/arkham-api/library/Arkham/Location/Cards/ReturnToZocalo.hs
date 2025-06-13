module Arkham.Location.Cards.ReturnToZocalo (returnToZocalo) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToZocalo = ReturnToZocalo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToZocalo :: LocationCard ReturnToZocalo
returnToZocalo = symbolLabel $ location ReturnToZocalo Cards.returnToZocalo 2 (Static 0)

instance HasAbilities ReturnToZocalo where
  getAbilities (ReturnToZocalo attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToZocalo where
  runMessage msg (ReturnToZocalo attrs) = runQueueT $ case msg of
    _ -> ReturnToZocalo <$> liftRunMessage msg attrs
