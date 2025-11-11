module Arkham.Location.Cards.HighRollersTableBusyNight (highRollersTableBusyNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HighRollersTableBusyNight = HighRollersTableBusyNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRollersTableBusyNight :: LocationCard HighRollersTableBusyNight
highRollersTableBusyNight = symbolLabel $ location HighRollersTableBusyNight Cards.highRollersTableBusyNight 0 (Static 0)

instance HasAbilities HighRollersTableBusyNight where
  getAbilities (HighRollersTableBusyNight attrs) =
    extendRevealed attrs []

instance RunMessage HighRollersTableBusyNight where
  runMessage msg (HighRollersTableBusyNight attrs) = runQueueT $ case msg of
    _ -> HighRollersTableBusyNight <$> liftRunMessage msg attrs
