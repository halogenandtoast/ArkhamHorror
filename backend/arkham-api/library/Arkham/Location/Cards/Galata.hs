module Arkham.Location.Cards.Galata (galata) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Galata = Galata LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

galata :: LocationCard Galata
galata = symbolLabel $ location Galata Cards.galata 3 (PerPlayer 1)

instance HasAbilities Galata where
  getAbilities (Galata attrs) =
    extendRevealed attrs []

instance RunMessage Galata where
  runMessage msg (Galata attrs) = runQueueT $ case msg of
    _ -> Galata <$> liftRunMessage msg attrs
