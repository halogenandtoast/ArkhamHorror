module Arkham.Location.Cards.TheKeyReliquarySanctum (theKeyReliquarySanctum) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheKeyReliquarySanctum = TheKeyReliquarySanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKeyReliquarySanctum :: LocationCard TheKeyReliquarySanctum
theKeyReliquarySanctum = location TheKeyReliquarySanctum Cards.theKeyReliquarySanctum 0 (Static 0)

instance HasAbilities TheKeyReliquarySanctum where
  getAbilities (TheKeyReliquarySanctum a) =
    extendRevealed a []

instance RunMessage TheKeyReliquarySanctum where
  runMessage msg (TheKeyReliquarySanctum attrs) = runQueueT $ case msg of
    _ -> TheKeyReliquarySanctum <$> liftRunMessage msg attrs
