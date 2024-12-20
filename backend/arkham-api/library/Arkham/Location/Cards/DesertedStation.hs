module Arkham.Location.Cards.DesertedStation (desertedStation) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DesertedStation = DesertedStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desertedStation :: LocationCard DesertedStation
desertedStation = location DesertedStation Cards.desertedStation 3 (PerPlayer 2)

instance HasAbilities DesertedStation where
  getAbilities (DesertedStation attrs) =
    extendRevealed attrs []

instance RunMessage DesertedStation where
  runMessage msg (DesertedStation attrs) = runQueueT $ case msg of
    _ -> DesertedStation <$> liftRunMessage msg attrs
