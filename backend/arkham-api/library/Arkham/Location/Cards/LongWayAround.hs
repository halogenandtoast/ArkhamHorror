module Arkham.Location.Cards.LongWayAround (longWayAround, LongWayAround (..)) where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LongWayAround = LongWayAround LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

longWayAround :: LocationCard LongWayAround
longWayAround =
  locationWith LongWayAround Cards.longWayAround 6 (Static 0)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities LongWayAround where
  getAbilities (LongWayAround attrs) =
    extendRevealed attrs []

instance RunMessage LongWayAround where
  runMessage msg (LongWayAround attrs) = runQueueT $ case msg of
    _ -> LongWayAround <$> liftRunMessage msg attrs
