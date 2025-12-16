module Arkham.Location.Cards.TheKeyReliquaryLair (theKeyReliquaryLair) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheKeyReliquaryLair = TheKeyReliquaryLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKeyReliquaryLair :: LocationCard TheKeyReliquaryLair
theKeyReliquaryLair = symbolLabel $ location TheKeyReliquaryLair Cards.theKeyReliquaryLair 0 (Static 0)

instance HasAbilities TheKeyReliquaryLair where
  getAbilities (TheKeyReliquaryLair a) =
    extendRevealed a []

instance RunMessage TheKeyReliquaryLair where
  runMessage msg (TheKeyReliquaryLair attrs) = runQueueT $ case msg of
    _ -> TheKeyReliquaryLair <$> liftRunMessage msg attrs
