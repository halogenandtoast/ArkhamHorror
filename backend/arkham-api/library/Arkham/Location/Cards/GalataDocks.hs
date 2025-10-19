module Arkham.Location.Cards.GalataDocks (galataDocks) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GalataDocks = GalataDocks LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

galataDocks :: LocationCard GalataDocks
galataDocks = symbolLabel $ location GalataDocks Cards.galataDocks 5 (PerPlayer 1)

instance HasAbilities GalataDocks where
  getAbilities (GalataDocks attrs) =
    extendRevealed attrs []

instance RunMessage GalataDocks where
  runMessage msg (GalataDocks attrs) = runQueueT $ case msg of
    _ -> GalataDocks <$> liftRunMessage msg attrs
