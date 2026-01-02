module Arkham.Location.Cards.TheCommonsDay (theCommonsDay) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheCommonsDay = TheCommonsDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCommonsDay :: LocationCard TheCommonsDay
theCommonsDay = symbolLabel $ location TheCommonsDay Cards.theCommonsDay 0 (Static 0)

instance HasAbilities TheCommonsDay where
  getAbilities (TheCommonsDay a) =
    extendRevealed a []

instance RunMessage TheCommonsDay where
  runMessage msg (TheCommonsDay attrs) = runQueueT $ case msg of
    _ -> TheCommonsDay <$> liftRunMessage msg attrs
