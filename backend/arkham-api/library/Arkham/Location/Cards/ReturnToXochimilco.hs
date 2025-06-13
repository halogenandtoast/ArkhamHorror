module Arkham.Location.Cards.ReturnToXochimilco (returnToXochimilco) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToXochimilco = ReturnToXochimilco LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToXochimilco :: LocationCard ReturnToXochimilco
returnToXochimilco = symbolLabel $ location ReturnToXochimilco Cards.returnToXochimilco 3 (Static 0)

instance HasAbilities ReturnToXochimilco where
  getAbilities (ReturnToXochimilco attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToXochimilco where
  runMessage msg (ReturnToXochimilco attrs) = runQueueT $ case msg of
    _ -> ReturnToXochimilco <$> liftRunMessage msg attrs
