module Arkham.Location.Cards.RouletteWheel (rouletteWheel) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RouletteWheel = RouletteWheel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rouletteWheel :: LocationCard RouletteWheel
rouletteWheel = symbolLabel $ location RouletteWheel Cards.rouletteWheel 0 (Static 0)

instance HasAbilities RouletteWheel where
  getAbilities (RouletteWheel attrs) =
    extendRevealed attrs []

instance RunMessage RouletteWheel where
  runMessage msg (RouletteWheel attrs) = runQueueT $ case msg of
    _ -> RouletteWheel <$> liftRunMessage msg attrs
