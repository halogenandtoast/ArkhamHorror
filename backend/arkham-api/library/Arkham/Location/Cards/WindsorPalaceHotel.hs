module Arkham.Location.Cards.WindsorPalaceHotel (windsorPalaceHotel) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WindsorPalaceHotel = WindsorPalaceHotel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

windsorPalaceHotel :: LocationCard WindsorPalaceHotel
windsorPalaceHotel = symbolLabel $ location WindsorPalaceHotel Cards.windsorPalaceHotel 0 (Static 0)

instance HasAbilities WindsorPalaceHotel where
  getAbilities (WindsorPalaceHotel a) =
    extendRevealed a []

instance RunMessage WindsorPalaceHotel where
  runMessage msg (WindsorPalaceHotel attrs) = runQueueT $ case msg of
    _ -> WindsorPalaceHotel <$> liftRunMessage msg attrs
