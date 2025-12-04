module Arkham.Location.Cards.KualaLumpurStationEastWing (kualaLumpurStationEastWing) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype KualaLumpurStationEastWing = KualaLumpurStationEastWing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kualaLumpurStationEastWing :: LocationCard KualaLumpurStationEastWing
kualaLumpurStationEastWing = symbolLabel $ location KualaLumpurStationEastWing Cards.kualaLumpurStationEastWing 0 (Static 0)

instance HasAbilities KualaLumpurStationEastWing where
  getAbilities (KualaLumpurStationEastWing a) =
    extendRevealed a []

instance RunMessage KualaLumpurStationEastWing where
  runMessage msg (KualaLumpurStationEastWing attrs) = runQueueT $ case msg of
    _ -> KualaLumpurStationEastWing <$> liftRunMessage msg attrs
