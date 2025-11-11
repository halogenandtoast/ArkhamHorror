module Arkham.Location.Cards.StaffAccessHallway (staffAccessHallway) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype StaffAccessHallway = StaffAccessHallway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

staffAccessHallway :: LocationCard StaffAccessHallway
staffAccessHallway = symbolLabel $ location StaffAccessHallway Cards.staffAccessHallway 0 (Static 0)

instance HasAbilities StaffAccessHallway where
  getAbilities (StaffAccessHallway attrs) =
    extendRevealed attrs []

instance RunMessage StaffAccessHallway where
  runMessage msg (StaffAccessHallway attrs) = runQueueT $ case msg of
    _ -> StaffAccessHallway <$> liftRunMessage msg attrs
