module Arkham.Location.Cards.TheGateOfYquaa (theGateOfYquaa) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheGateOfYquaa = TheGateOfYquaa LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGateOfYquaa :: LocationCard TheGateOfYquaa
theGateOfYquaa = location TheGateOfYquaa Cards.theGateOfYquaa 1 (PerPlayer 1)

instance HasAbilities TheGateOfYquaa where
  getAbilities (TheGateOfYquaa attrs) =
    extendRevealed attrs []

instance RunMessage TheGateOfYquaa where
  runMessage msg (TheGateOfYquaa attrs) = runQueueT $ case msg of
    _ -> TheGateOfYquaa <$> liftRunMessage msg attrs
