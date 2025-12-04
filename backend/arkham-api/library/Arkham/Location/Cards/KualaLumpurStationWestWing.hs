module Arkham.Location.Cards.KualaLumpurStationWestWing (kualaLumpurStationWestWing) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype KualaLumpurStationWestWing = KualaLumpurStationWestWing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kualaLumpurStationWestWing :: LocationCard KualaLumpurStationWestWing
kualaLumpurStationWestWing = symbolLabel $ location KualaLumpurStationWestWing Cards.kualaLumpurStationWestWing 0 (Static 0)

instance HasAbilities KualaLumpurStationWestWing where
  getAbilities (KualaLumpurStationWestWing a) =
    extendRevealed a []

instance RunMessage KualaLumpurStationWestWing where
  runMessage msg (KualaLumpurStationWestWing attrs) = runQueueT $ case msg of
    _ -> KualaLumpurStationWestWing <$> liftRunMessage msg attrs
