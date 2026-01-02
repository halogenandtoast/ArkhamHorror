module Arkham.Location.Cards.BoardingHouseNight (boardingHouseNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BoardingHouseNight = BoardingHouseNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boardingHouseNight :: LocationCard BoardingHouseNight
boardingHouseNight = symbolLabel $ location BoardingHouseNight Cards.boardingHouseNight 0 (Static 0)

instance HasAbilities BoardingHouseNight where
  getAbilities (BoardingHouseNight a) =
    extendRevealed a []

instance RunMessage BoardingHouseNight where
  runMessage msg (BoardingHouseNight attrs) = runQueueT $ case msg of
    _ -> BoardingHouseNight <$> liftRunMessage msg attrs
