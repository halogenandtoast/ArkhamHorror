module Arkham.Location.Cards.BoardingHouseDay (boardingHouseDay) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BoardingHouseDay = BoardingHouseDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boardingHouseDay :: LocationCard BoardingHouseDay
boardingHouseDay = symbolLabel $ location BoardingHouseDay Cards.boardingHouseDay 0 (Static 0)

instance HasAbilities BoardingHouseDay where
  getAbilities (BoardingHouseDay a) =
    extendRevealed a []

instance RunMessage BoardingHouseDay where
  runMessage msg (BoardingHouseDay attrs) = runQueueT $ case msg of
    _ -> BoardingHouseDay <$> liftRunMessage msg attrs
