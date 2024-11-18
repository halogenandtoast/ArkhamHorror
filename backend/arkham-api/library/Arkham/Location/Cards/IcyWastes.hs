module Arkham.Location.Cards.IcyWastes (icyWastes, IcyWastes (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype IcyWastes = IcyWastes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icyWastes :: LocationCard IcyWastes
icyWastes = symbolLabel $ location IcyWastes Cards.icyWastes 0 (Static 0)

instance HasAbilities IcyWastes where
  getAbilities (IcyWastes attrs) =
    extendRevealed attrs []

instance RunMessage IcyWastes where
  runMessage msg (IcyWastes attrs) = runQueueT $ case msg of
    _ -> IcyWastes <$> liftRunMessage msg attrs
