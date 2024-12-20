module Arkham.Location.Cards.TheBlackStone (theBlackStone) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheBlackStone = TheBlackStone LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackStone :: LocationCard TheBlackStone
theBlackStone = location TheBlackStone Cards.theBlackStone 4 (PerPlayer 2)

instance HasAbilities TheBlackStone where
  getAbilities (TheBlackStone attrs) =
    extendRevealed attrs []

instance RunMessage TheBlackStone where
  runMessage msg (TheBlackStone attrs) = runQueueT $ case msg of
    _ -> TheBlackStone <$> liftRunMessage msg attrs
