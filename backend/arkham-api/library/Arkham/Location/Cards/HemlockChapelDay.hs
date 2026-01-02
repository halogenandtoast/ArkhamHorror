module Arkham.Location.Cards.HemlockChapelDay (hemlockChapelDay) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HemlockChapelDay = HemlockChapelDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hemlockChapelDay :: LocationCard HemlockChapelDay
hemlockChapelDay = symbolLabel $ location HemlockChapelDay Cards.hemlockChapelDay 0 (Static 0)

instance HasAbilities HemlockChapelDay where
  getAbilities (HemlockChapelDay a) =
    extendRevealed a []

instance RunMessage HemlockChapelDay where
  runMessage msg (HemlockChapelDay attrs) = runQueueT $ case msg of
    _ -> HemlockChapelDay <$> liftRunMessage msg attrs
