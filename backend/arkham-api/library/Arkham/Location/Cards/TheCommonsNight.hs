module Arkham.Location.Cards.TheCommonsNight (theCommonsNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheCommonsNight = TheCommonsNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCommonsNight :: LocationCard TheCommonsNight
theCommonsNight = symbolLabel $ location TheCommonsNight Cards.theCommonsNight 0 (Static 0)

instance HasAbilities TheCommonsNight where
  getAbilities (TheCommonsNight a) =
    extendRevealed a []

instance RunMessage TheCommonsNight where
  runMessage msg (TheCommonsNight attrs) = runQueueT $ case msg of
    _ -> TheCommonsNight <$> liftRunMessage msg attrs
