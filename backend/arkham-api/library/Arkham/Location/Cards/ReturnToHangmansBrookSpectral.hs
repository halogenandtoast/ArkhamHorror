module Arkham.Location.Cards.ReturnToHangmansBrookSpectral (returnToHangmansBrookSpectral) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToHangmansBrookSpectral = ReturnToHangmansBrookSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToHangmansBrookSpectral :: LocationCard ReturnToHangmansBrookSpectral
returnToHangmansBrookSpectral = location ReturnToHangmansBrookSpectral Cards.returnToHangmansBrookSpectral 3 (Static 0)

instance HasAbilities ReturnToHangmansBrookSpectral where
  getAbilities (ReturnToHangmansBrookSpectral attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToHangmansBrookSpectral where
  runMessage msg (ReturnToHangmansBrookSpectral attrs) = runQueueT $ case msg of
    _ -> ReturnToHangmansBrookSpectral <$> liftRunMessage msg attrs
