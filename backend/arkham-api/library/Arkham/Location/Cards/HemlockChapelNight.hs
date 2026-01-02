module Arkham.Location.Cards.HemlockChapelNight (hemlockChapelNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HemlockChapelNight = HemlockChapelNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hemlockChapelNight :: LocationCard HemlockChapelNight
hemlockChapelNight = symbolLabel $ location HemlockChapelNight Cards.hemlockChapelNight 0 (Static 0)

instance HasAbilities HemlockChapelNight where
  getAbilities (HemlockChapelNight a) =
    extendRevealed a []

instance RunMessage HemlockChapelNight where
  runMessage msg (HemlockChapelNight attrs) = runQueueT $ case msg of
    _ -> HemlockChapelNight <$> liftRunMessage msg attrs
