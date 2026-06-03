module Arkham.Location.Cards.TheAbyssSpiralingOblivion (theAbyssSpiralingOblivion) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheAbyssSpiralingOblivion = TheAbyssSpiralingOblivion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAbyssSpiralingOblivion :: LocationCard TheAbyssSpiralingOblivion
theAbyssSpiralingOblivion = location TheAbyssSpiralingOblivion Cards.theAbyssSpiralingOblivion 5 (Static 2)

instance RunMessage TheAbyssSpiralingOblivion where
  runMessage msg (TheAbyssSpiralingOblivion attrs) =
    runQueueT $ TheAbyssSpiralingOblivion <$> liftRunMessage msg attrs
